# Two stake pools, each a VM running cardano-node 11.2.1 (its own NixOS
# module) and dmq-node (ours).
{ pkgs, dmqModule, cardanoNodeModule, cardanoNodePackages, dmq-client }:

let
  inherit (pkgs) lib;

  # create-env stamps the build time; this fixed instant replaces it and the
  # test sets both VM clocks to it.
  systemStartPosix = 1893456000;
  testnetMagic = 42;
  dmqMagic = 4242;
  cardanoPort = 3001;
  dmqPort = 3141;

  env = pkgs.callPackage ./testnet-env.nix {
    inherit (cardanoNodePackages) cardano-testnet cardano-cli;
    inherit systemStartPosix testnetMagic;
  };

  # create-env writes the cardano-config envelope. services.cardano-node takes
  # a flat config and envelopes it again (configFormat = "enveloped").
  envelope = (lib.importJSON "${env}/configuration.json").Configuration;
  withStorePaths = lib.mapAttrs (key: value:
    if lib.hasSuffix "GenesisFile" key then "${env}/${value}" else value);
  nodeConfig =
    withStorePaths (envelope.Protocol // envelope.Testing)
    // envelope.Network
    // envelope.HermodTracing
    // { Protocol = "Cardano"; };

  shelleyGenesis = lib.importJSON "${env}/shelley-genesis.json";

  body = name: pkgs.writeText "body-${name}" (lib.concatStrings (lib.replicate 64 "${name}-msg/"));

  pool = name: peer: { config, nodes, ... }:
    let
      topologyFor = port: {
        localRoots = [{
          accessPoints = [{ address = nodes.${peer}.networking.primaryIPAddress; inherit port; }];
          advertise = false;
          trustable = true;
          valency = 1;
        }];
        publicRoots = [ ];
        useLedgerAfterSlot = -1;
      };
    in
    {
      imports = [ cardanoNodeModule dmqModule ];

      virtualisation = {
        memorySize = 3072;
        cores = 2;
      };

      networking.firewall.allowedTCPPorts = [ cardanoPort ];
      environment.systemPackages = [ cardanoNodePackages.cardano-cli dmq-client pkgs.jq ];

      services = {
        # The test owns the clock.
        timesyncd.enable = false;

        cardano-node = {
          enable = true;
          inherit cardanoNodePackages nodeConfig;
          configFormat = "enveloped";
          # The module writes its config with builtins.toFile, which cannot refer to
          # the generated env; production genesis paths are sources, not derivations.
          nodeConfigFile = "${pkgs.writeText "cardano-node-config.json"
          (builtins.toJSON (cardanoNodePackages.cardanoLib.mkEnvelope nodeConfig))}";
          hostAddr = "0.0.0.0";
          port = cardanoPort;
          kesKey = "/var/lib/pool-keys/kes.skey";
          vrfKey = "/var/lib/pool-keys/vrf.skey";
          operationalCertificate = "/var/lib/pool-keys/opcert.cert";
          topology = builtins.toFile "topology.json" (builtins.toJSON (topologyFor cardanoPort));
        };

        dmq-node = {
          enable = true;
          networkMagic = dmqMagic;
          cardanoNodeConfig = config.services.cardano-node.nodeConfig;
          cardanoNodeSocketPath = config.services.cardano-node.socketPath 0;
          hostAddr = config.networking.primaryIPAddress;
          port = dmqPort;
          openFirewall = true;
          topology = topologyFor dmqPort;
          traceOptions."" = {
            backends = [ "Stdout MachineFormat" ];
            severity = "Info";
          };
        };
      };

      systemd.services = {
        # cardano-node refuses signing keys readable by others, and store paths are.
        pool-keys = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            install -d -m 0700 -o cardano-node -g cardano-node /var/lib/pool-keys
            for key in kes.skey vrf.skey opcert.cert; do
              install -m 0400 -o cardano-node -g cardano-node \
                ${env}/pools-keys/${name}/$key /var/lib/pool-keys/$key
            done
          '';
        };

        cardano-node = {
          # Started by the test script once the clock is on the genesis instant.
          wantedBy = lib.mkForce [ ];
          requires = [ "pool-keys.service" ];
          after = [ "pool-keys.service" ];
          # The node socket must be writable by group cardano-node, which dmq-node
          # joins.
          serviceConfig.UMask = "0007";
        };

        dmq-node.wantedBy = lib.mkForce [ ];
      };
    };
in
pkgs.testers.runNixOSTest {
  name = "dmq-node";
  globalTimeout = 1500;

  nodes = {
    pool1 = pool "pool1" "pool2";
    pool2 = pool "pool2" "pool1";
  };

  testScript = { nodes, ... }: ''
    import json
    import time

    pools = [pool1, pool2]
    cardano_socket = "${nodes.pool1.services.cardano-node.socketPath 0}"
    dmq_socket = "${nodes.pool1.services.dmq-node.socketPath}"
    keys = {
        "pool1": "${env}/pools-keys/pool1",
        "pool2": "${env}/pools-keys/pool2",
        "unregistered": "${env}/unregistered",
    }
    bodies = {
        "pool1": "${body "pool1"}",
        "pool2": "${body "pool2"}",
        "pool1-again": "${body "pool1-again"}",
        "pool2-again": "${body "pool2-again"}",
    }
    slots_per_kes_period = ${toString shelleyGenesis.slotsPerKESPeriod}

    # dmq-node answers these until its cardano-node has a stake snapshot with
    # the pool in it.
    not_ready_yet = ("NotInitialized", "PoolNotEligible", "UnrecognizedPool")

    def tip(machine):
        return json.loads(machine.succeed(
            f"cardano-cli latest query tip --testnet-magic ${toString testnetMagic} --socket-path {cardano_socket}"
        ))

    def submit(machine, key, body):
        kes_period = tip(machine)["slot"] // slots_per_kes_period
        return machine.execute(
            f"dmq-client submit --socket {dmq_socket} --network-magic ${toString dmqMagic}"
            f" --kes-signing-key {keys[key]}/kes.skey --op-cert {keys[key]}/opcert.cert"
            f" --kes-period {kes_period} --expires-at $(( $(date +%s) + 900 ))"
            f" --body {bodies[body]} 2>&1"
        )

    def submit_until_accepted(machine, key, body, retry_on):
        deadline = time.monotonic() + 300
        while True:
            status, out = submit(machine, key, body)
            if status == 0:
                return out.strip()
            assert any(reason in out for reason in retry_on), f"{key} rejected: {out}"
            assert time.monotonic() < deadline, f"{key} still rejected after 300s: {out}"
            time.sleep(2)

    def notified_ids(path):
        return [json.loads(line)["id"] for line in path.splitlines()]

    start_all()
    for machine in pools:
        machine.wait_for_unit("pool-keys.service")

    with subtest("both VM clocks on the genesis start"):
        t0 = time.time()
        for machine in pools:
            machine.succeed(f"date -u -s @{${toString systemStartPosix} - 5 + (time.time() - t0):.3f}")
        for machine in pools:
            machine.succeed("systemctl start cardano-node.service dmq-node.service")

    with subtest("both pools forge on one chain"):
        for machine in pools:
            machine.wait_until_succeeds(
                f"cardano-cli latest query tip --testnet-magic ${toString testnetMagic} --socket-path {cardano_socket}"
                " | jq -e '.block >= 10'",
                timeout=300,
            )
            machine.wait_until_succeeds(
                "journalctl -u cardano-node.service -o cat | grep -q Forge.Loop.AdoptedBlock",
                timeout=120,
            )
        assert tip(pool1)["era"] == "Conway", tip(pool1)

    with subtest("each dmq-node reads its cardano-node and peers with the other"):
        for machine in pools:
            machine.wait_until_succeeds(
                "journalctl -u dmq-node.service -o cat | grep -q Net.Local.LocalStateQuery.CurrentEpoch",
                timeout=120,
            )
            machine.wait_until_succeeds(
                "journalctl -u dmq-node.service -o cat | grep -q PromoteWarmDone",
                timeout=120,
            )
            machine.succeed(f"test \"$(stat -c %G {dmq_socket})\" = dmq-node")
            machine.succeed(
                "ps -o supgrp= -p \"$(systemctl show -p MainPID --value dmq-node.service)\""
                " | tr , '\\n' | grep -qx cardano-node"
            )

    with subtest("each pool's first message is accepted and reaches both dmq-nodes"):
        id1 = submit_until_accepted(pool1, "pool1", "pool1", not_ready_yet)
        id2 = submit_until_accepted(pool2, "pool2", "pool2", not_ready_yet)
        for machine in pools:
            out = machine.succeed(
                f"timeout 120 dmq-client notify --socket {dmq_socket} --network-magic ${toString dmqMagic} --count 2"
            )
            assert sorted(notified_ids(out)) == sorted([id1, id2]), out

    with subtest("later messages arrive once each, in order"):
        pool2.succeed(
            f"(timeout 300 dmq-client notify --socket {dmq_socket} --network-magic ${toString dmqMagic}"
            " --count 4 > /tmp/notify 2>&1 &)"
        )
        # dmq-node accepts one message per pool per minute. A resend of id3
        # would show up before id4, so the fourth message proves id3 came once.
        id3 = submit_until_accepted(pool1, "pool1", "pool1-again", ("SigTooFrequent",))
        # id3 travels from pool1; let it land before pool2 adds id4 locally.
        pool2.wait_until_succeeds(f"grep -q {id3} /tmp/notify", timeout=120)
        id4 = submit_until_accepted(pool2, "pool2", "pool2-again", ("SigTooFrequent",))
        pool2.wait_until_succeeds("test $(wc -l < /tmp/notify) -ge 4", timeout=120)
        ids = notified_ids(pool2.succeed("cat /tmp/notify"))
        assert len(ids) == 4 and sorted(ids[:2]) == sorted([id1, id2]) and ids[2:] == [id3, id4], ids

    with subtest("a pool outside the stake distribution is rejected"):
        status, out = submit(pool1, "unregistered", "pool1")
        assert status != 0 and "UnrecognizedPool" in out, out
  '';
}
