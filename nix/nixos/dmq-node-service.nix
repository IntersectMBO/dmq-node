# NixOS service module for dmq-node.
#
# Self-contained: it references nothing from this flake, so it can be imported
# by path.
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption mkPackageOption types;

  cfg = config.services.dmq-node;
  json = pkgs.formats.json { };
  # json.type also accepts a path, which json.generate writes as a string.
  jsonObject = types.attrsOf json.type;

  # The unit's only writable path under ProtectSystem = "strict".
  runtimeDirectory = "dmq-node";

  # User and group the module creates, and only while they are the defaults.
  defaultAccount = "dmq-node";

  requiredNodeConfigKey = key:
    cfg.cardanoNodeConfig.${key} or (throw
      "services.dmq-node.cardanoNodeConfig has no `${key}`; it must be a flat cardano-node config, such as services.cardano-node.nodeConfig or cardanoLib.environments.<env>.nodeConfig");

  # cardano-node resolves a relative genesis path against its config file,
  # whose location an attrset no longer carries.
  shelleyGenesisFile =
    let file = requiredNodeConfigKey "ShelleyGenesisFile";
    in
    if types.path.check file then file
    else
      throw
        "services.dmq-node.cardanoNodeConfig.ShelleyGenesisFile must be an absolute path, got `${file}`; override it, e.g. `// { ShelleyGenesisFile = ./${file}; }`";
  shelleyGenesis = builtins.fromJSON (builtins.readFile shelleyGenesisFile);

  generatedSettings = {
    NetworkMagic = cfg.networkMagic;
    CardanoNetworkMagic = shelleyGenesis.networkMagic;
    ShelleyGenesisFile = "${shelleyGenesisFile}";
    # Optional for cardano-node and dmq-node alike: without it, neither
    # verifies the genesis file.
    ShelleyGenesisHash = cfg.cardanoNodeConfig.ShelleyGenesisHash or null;
    IPv4 = cfg.hostAddr;
    IPv6 = cfg.ipv6HostAddr;
    PortNumber = cfg.port;
    LocalAddress = cfg.socketPath;
    CardanoNodeSocket = cfg.cardanoNodeSocketPath;
    TraceOptions = cfg.traceOptions;
  };

  # Sandbox settings the unit enables, with the directories each one hides.
  sandboxHides = {
    PrivateTmp = [ "/tmp/" "/var/tmp/" ];
    ProtectHome = [ "/home/" "/root/" "/run/user/" ];
  };
  hiddenSocketDirs = lib.filter
    (dir: lib.hasPrefix dir cfg.cardanoNodeSocketPath)
    (lib.concatLists (lib.attrValues sandboxHides));

  # Ports below 1024 need the capability under the kernel's default
  # ip_unprivileged_port_start. A string, not a list: NixOS writes no line for
  # an empty list, while an empty assignment is systemd's empty set.
  capabilities = lib.optionalString (cfg.port < 1024) "CAP_NET_BIND_SERVICE";

  duplicatedKeys = lib.intersectLists
    (lib.attrNames generatedSettings)
    (lib.attrNames cfg.extraSettings);

  configFile = json.generate "dmq-node-config.json"
    (lib.filterAttrs (_: v: v != null) generatedSettings // cfg.extraSettings);

  topologyFile = json.generate "dmq-node-topology.json" cfg.topology;

  # dmq-node exits when it cannot reach cardano-node, which only listens once
  # its ledger replay is done (hours on mainnet). Waiting here, as the main
  # process, keeps the unit active and out of systemd's start limits.
  # Connecting, not just testing for the file: a stopped node leaves its socket
  # file behind. Connecting needs write permission, which no wait grants, so a
  # socket we cannot write fails the start instead.
  dmqNode = pkgs.writeShellApplication {
    name = "dmq-node-when-cardano-node-listens";
    runtimeInputs = [ pkgs.socat ];
    text = ''
      socket=${lib.escapeShellArg cfg.cardanoNodeSocketPath}
      connect() {
        if [ -S "$socket" ] && [ ! -w "$socket" ]; then
          echo "$socket is not writable by group ${cfg.cardanoNodeSocketGroup}; see services.dmq-node.cardanoNodeSocketGroup" >&2
          exit 1
        fi
        socat -u /dev/null "UNIX-CONNECT:$socket" 2>/dev/null
      }
      if ! connect; then
        echo "Waiting for cardano-node to listen on $socket ..."
        until connect; do
          sleep 5
        done
      fi
      exec ${lib.escapeShellArgs [
        (lib.getExe' cfg.package "dmq-node")
        "--configuration-file"
        configFile
        "--topology-file"
        topologyFile
        "+RTS"
        "-T"
        "-RTS"
      ]}
    '';
  };
in
{
  options.services.dmq-node = {
    enable = mkEnableOption "the DMQ node";

    package = mkPackageOption pkgs "dmq-node" { };

    networkMagic = mkOption {
      type = types.ints.u32;
      description = "Network magic of the DMQ network (CIP-137); mainnet is 2912307721.";
    };

    cardanoNodeConfig = mkOption {
      type = types.attrs;
      example = lib.literalExpression "config.services.cardano-node.nodeConfig";
      description = ''
        The flat cardano-node configuration of the node this DMQ node attaches
        to. `ShelleyGenesisFile` and, if present, `ShelleyGenesisHash` are read
        from it, and the cardano network magic is read from that genesis file.

        cardano-node keeps this attrset flat under both `configFormat` dialects
        and envelopes it only when writing its config file.

        `services.cardano-node.nodeConfig` is the base configuration only. If
        the node overrides its genesis through `extraNodeConfig` or
        `extraNodeInstanceConfig`, pass the merged attrset instead:
        `lib.recursiveUpdate (lib.recursiveUpdate nodeConfig extraNodeConfig) (extraNodeInstanceConfig 0)`.
        A `nodeConfigFile` replaces all three.
      '';
    };

    cardanoNodeSocketPath = mkOption {
      type = types.str;
      example = lib.literalExpression "config.services.cardano-node.socketPath 0";
      description = ''
        Node-to-client socket of the cardano-node. The service waits until
        cardano-node accepts connections on it before starting dmq-node,
        which exits when that connection fails.
      '';
    };

    cardanoNodeSocketGroup = mkOption {
      type = types.str;
      default = "cardano-node";
      description = ''
        Group owning the cardano-node socket; the service joins it as a
        supplementary group to connect. The default is the group that
        cardano-node's NixOS module runs as, so its socket has that group.

        Connecting needs the socket to be group-writable, which it is not by
        default (mode 0755). Make it so on the cardano-node side with one of:
        - `services.cardano-node.systemdSocketActivation = true`
          (socket mode 0660 with `services.cardano-node.socketGroup`);
        - `services.cardano-node.shareNodeSocket = true` under cardano-parts;
        - `systemd.services.cardano-node.serviceConfig.UMask = "0007"`.

        The service fails to start while the socket is not writable.
      '';
    };

    hostAddr = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "0.0.0.0";
      description = "IPv4 address to bind node-to-node connections to.";
    };

    ipv6HostAddr = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "::";
      description = "IPv6 address to bind node-to-node connections to.";
    };

    port = mkOption {
      type = types.port;
      default = 3141;
      description = "Node-to-node TCP port.";
    };

    socketPath = mkOption {
      type = types.str;
      readOnly = true;
      default = "/run/${runtimeDirectory}/dmq-node.socket";
      description = ''
        Node-to-client socket that DMQ clients connect to. Fixed inside the
        service's runtime directory, the only path the unit may write.
      '';
    };

    topology = mkOption {
      type = jsonObject;
      example = {
        localRoots = [{
          accessPoints = [{ address = "dmq.example.org"; port = 3141; }];
          advertise = false;
          trustable = true;
          valency = 1;
        }];
        publicRoots = [ ];
        useLedgerAfterSlot = -1;
      };
      description = ''
        Peers to connect to, in cardano-node's topology syntax:
        <https://developers.cardano.org/docs/get-started/infrastructure/node/topology/>.
        To use a topology file, pass `lib.importJSON ./topology.json`.

        The access points are other dmq-nodes (port 3141 by default), not
        cardano-nodes. `bootstrapPeers` and `peerSnapshotFile` are not
        supported. Ledger peers additionally need `LedgerPeers = true` in
        `extraSettings` and SRV relay records (CIP-155); see the dmq-node
        README.
      '';
    };

    traceOptions = mkOption {
      type = jsonObject;
      example = {
        "" = {
          backends = [ "Stdout MachineFormat" "PrometheusSimple suffix 127.0.0.1 12800" ];
          severity = "Info";
        };
        "Net.PeerSelection" = { severity = "Debug"; };
      };
      description = ''
        trace-dispatcher `TraceOptions`: per trace namespace, its `severity`,
        `detail`, `maxFrequency` and `backends`. The `""` namespace sets the
        defaults the others refine. See the Configuration section of
        <https://github.com/IntersectMBO/hermod-tracing/blob/master/trace-dispatcher/doc/trace-dispatcher.md#configuration>.

        dmq-node requires this option. The example serves DMQ metrics
        (`dmq_node_*`) and GHC runtime statistics (`rts_gc_*`) at
        `http://127.0.0.1:12800/metrics`.
      '';
    };

    user = mkOption {
      type = types.str;
      default = defaultAccount;
      description = ''
        User the service runs as. The module creates it only under the
        default name; another name must be an existing account.
      '';
    };

    group = mkOption {
      type = types.str;
      default = defaultAccount;
      description = ''
        Group the service runs as. Its members get access to the
        node-to-client socket. The module creates it only under the default
        name.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open `port` in the firewall.";
    };

    extraSettings = mkOption {
      type = jsonObject;
      default = { };
      example = { LedgerPeers = true; PeerSharing = false; };
      description = ''
        Further keys of the dmq-node configuration file. Keys that the options
        above already set are rejected.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.hostAddr != null || cfg.ipv6HostAddr != null;
        message = "services.dmq-node: set hostAddr, ipv6HostAddr or both.";
      }
      {
        assertion = hiddenSocketDirs == [ ];
        message = "services.dmq-node.cardanoNodeSocketPath is under ${toString hiddenSocketDirs}, which the service's sandbox hides; move the cardano-node socket, e.g. to /run/cardano-node/node.socket.";
      }
      {
        assertion = duplicatedKeys == [ ];
        message = "services.dmq-node.extraSettings sets keys owned by other options: ${toString duplicatedKeys}";
      }
    ];

    users = {
      users = mkIf (cfg.user == defaultAccount) {
        ${defaultAccount} = {
          isSystemUser = true;
          inherit (cfg) group;
        };
      };
      groups = mkIf (cfg.group == defaultAccount) {
        ${defaultAccount} = { };
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.services.dmq-node = {
      description = "DMQ node";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      # dmq-node also exits when a running cardano-node restarts. Every such
      # restart is a start here, so no start limit: a bounded one would leave
      # dmq-node failed after a few node restarts.
      startLimitIntervalSec = 0;

      serviceConfig = lib.mapAttrs (_: _: true) sandboxHides // {
        ExecStart = lib.getExe dmqNode;
        Restart = "always";
        RestartSec = "10s";

        User = cfg.user;
        Group = cfg.group;
        SupplementaryGroups = [ cfg.cardanoNodeSocketGroup ];
        NoNewPrivileges = true;
        AmbientCapabilities = capabilities;
        CapabilityBoundingSet = capabilities;
        RuntimeDirectory = runtimeDirectory;
        RuntimeDirectoryMode = "0750";
        # The node-to-client socket must be group-writable for clients.
        UMask = "0007";

        ProtectSystem = "strict";
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        LockPersonality = true;
      };
    };
  };
}
