# A two-pool cardano testnet from `cardano-testnet create-env`: Conway from
# epoch 0, with its genesis files, config and pool keys.
#
# create-env stamps systemStart with the build time. A cached build would start
# the chain days in the past, so both genesis start times are rewritten to
# `systemStartPosix` and the VM test sets its clocks to that instant.
{ runCommand, jq, cardano-testnet, cardano-cli, systemStartPosix, testnetMagic }:

runCommand "dmq-testnet-env"
{
  nativeBuildInputs = [ cardano-testnet cardano-cli jq ];
  inherit systemStartPosix testnetMagic;
} ''
  export HOME=$TMPDIR
  export CARDANO_CLI=${cardano-cli}/bin/cardano-cli
  cardano-testnet create-env --num-pool-nodes 2 --testnet-magic "$testnetMagic" --output env
  cd env

  jq --arg t "$(date -u -d "@$systemStartPosix" +%Y-%m-%dT%H:%M:%SZ)" '.systemStart = $t' shelley-genesis.json > s.json
  mv s.json shelley-genesis.json
  jq --argjson t "$systemStartPosix" '.startTime = $t' byron-genesis.json > b.json
  mv b.json byron-genesis.json

  shelley=$(cardano-cli hash genesis-file --genesis shelley-genesis.json)
  byron=$(cardano-cli byron genesis print-genesis-hash --genesis-json byron-genesis.json)
  jq --arg s "$shelley" --arg b "$byron" \
    '.Configuration.Protocol.ShelleyGenesisHash = $s | .Configuration.Protocol.ByronGenesisHash = $b' \
    configuration.yaml > configuration.json

  # A pool that is in no stake distribution, for the rejection case.
  mkdir unregistered
  cardano-cli latest node key-gen \
    --cold-verification-key-file unregistered/cold.vkey \
    --cold-signing-key-file unregistered/cold.skey \
    --operational-certificate-issue-counter-file unregistered/opcert.counter
  cardano-cli latest node key-gen-KES \
    --verification-key-file unregistered/kes.vkey \
    --signing-key-file unregistered/kes.skey
  cardano-cli latest node issue-op-cert \
    --kes-verification-key-file unregistered/kes.vkey \
    --cold-signing-key-file unregistered/cold.skey \
    --operational-certificate-issue-counter-file unregistered/opcert.counter \
    --kes-period 0 \
    --out-file unregistered/opcert.cert

  mkdir -p $out
  cp -r configuration.json *-genesis.json pools-keys unregistered $out/
''
