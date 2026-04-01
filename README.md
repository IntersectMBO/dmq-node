[![x86\_64-linux](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-dmq-node/main/x86_64-linux.required/shield&style=for-the-badge&label=x86_64-linux)](https://ci.iog.io/job/IntersectMBO-dmq-node/main/x86_64-linux.required)
[![x86\_64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-dmq-node/main/x86_64-darwin.required/shield&style=for-the-badge&label=x86_64-darwin)](https://ci.iog.io/job/IntersectMBO-dmq-node/main/x86_64-darwin.required)
[![aarch64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-dmq-node/main/aarch64-darwin.required/shield&style=for-the-badge&label=aarch64-linux)](https://ci.iog.io/job/IntersectMBO-dmq-node/main/aarch64-darwin.required)
[![mingw64](https://img.shields.io/github/actions/workflow/status/intersectmbo/dmq-node/build.yml?branch=main&label=mingw64&style=for-the-badge)](https://github.com/intersectmbo/dmq-node/actions/workflows/build.yml)
[![Haddocks](https://img.shields.io/github/actions/workflow/status/intersectmbo/dmq-node/github-page.yml?branch=main&label=Haddocks&style=for-the-badge)](https://dmq-node.cardano.intersectmbo.org/)
[![Discord](https://img.shields.io/discord/1136727663583698984?style=for-the-badge&color=blue)](https://discord.com/channels/1136727663583698984/1239889324745429122)

# Decentralized Message Queue

The DMQ node allows for client peers to communicate efficiently by publishing
and consuming messages which are diffused over a P2P network to other nodes.

This repository provides the `dmq-node` executable to participate in the DMQ
network.

The `dmq-node` is developed with respect to the [CIP#137].

__NOTE__: This is still an early version of DMQ node, which comes with some
quirks:

* [issue#6] - experimental support for ledger peers see [Ledger Peers](#ledger-peers) below.

# Instructions

## Building the project

We use cabal to build our project, potentially inside a Nix shell (nix develop or nix-shell). It should suffice with:
```bash
> cabal build dmq-node:exe:dmq-node
```

The executable can be run with:
```bash
> cabal run dmq-node
```

### Building with Nix

To build the executable using Nix, one can use:
```bash
> nix build .#dmq-node
```

#### Static build

To get a statically linked executable using [musl] library, use:
```bash
> nix build .#dmq-node-static
```

#### Docker image

To build a docker image:
```bash
> nix build .#docker-dmq
```

## Running

### Usage
```
Usage: dmq-node [--host-addr IPv4] [--host-ipv6-addr IPv6] 
                [-p|--port Port Number] [--local-socket FILENAME] 
                [-c|--configuration-file FILENAME] [-t|--topology-file FILENAME]
                [--cardano-node-socket Cardano node socket path] 
                [--cardano-network-magic Cardano node network magic] 
                [--dmq-network-magic dmq node network magic] [-v|--version]

  Run the DMQ-Node

Available options:
  --host-addr IPv4         IPv4 that the node will bind to
  --host-ipv6-addr IPv6    IPv6 that the node will bind to
  -p,--port Port Number    Port Number that the node will bind to
                           (default: 3141)
  --local-socket FILENAME  Unix socket for node-to-client communication
                           (default: "dmq-node.socket")
  -c,--configuration-file FILENAME
                           Configuration file for DMQ Node
                           (default: "dmq.config.json")
  -t,--topology-file FILENAME
                           Topology file for DMQ Node
                           (default: "dmq.topology.json")
  --cardano-node-socket Cardano node socket path
                           Used for local connections to Cardano node
                           (default: "cardano-node.socket")
  --cardano-network-magic Cardano node network magic
                           The network magic of cardano-node client for local
                           connections
                           (default: 764824073)
  --dmq-network-magic dmq node network magic
                           The network magic of the dmq network
                           (default: 3141592)
  -v,--version             Show dmq-node version
  -h,--help                Show this help text
```

### Configuration

`dmq-node` has default options for allmost all configuration values,
[ref][defaultConfiguration].   You only need to create a configuration file if
you want to modify one of the options.  To get a `json` key  for
a configuration option just remove the `dmqc` prefix from a `Configuration`
record field (e.g. `dmqcLedgerPeers → LedgerPeers`).

### Topology File

Topology file has the same syntax as for `cardano-node`, see [the original
documentation][topology-file].  However, the fields:  `bootstrapPeers` and
`peerSnapshotFile` are not supported.

### Ledger Peers

To use ledger peers, as `cardano-node` does there are additional requrements:

* You need `cardano-node-10.7` or newer to support ledger peer snapshot query
  over `cardano-node`'s node-to-client protocol.
* You need to configure `cardano-node` & `dmq-node` to use SRV records according to [CIP#155]
* You need to set `LedgerPeers: true` in the configuration file.

Currently ledger peers are disabled by default, but in a near future we will
enable them by default.

### Tracing

`dmq-node` is using `trace-dispatcher` framework for logging (i.e. the same as
the new tracing system for `cardano-node`).

### Configuration example

Note: it requires `cardano-node-10.7.x` for `LedgerPeers`, if you're running it
against an earlier `cardano-node` turn the option off.

The `Prometheus` requires a different port number than `cardano-node`'s
`Prometheus`.

```json
{ "LedgerPeers": true,
  "CardanoNodeSocket": "/var/run/cardano-node.socket",
  "TraceOptions": {
    "": {
      "backends": [
        "Stdout MachineFormat",
        "PrometheusSimple suffix 127.0.0.1 12800"
      ],
      "severity": "Info"
    }
  }
}
```

## Developing with Nix

To enter a development shell with all dependencies available, use:
```bash
> nix develop
```

## Testing the project

To run the test suite, one can use:

``` bash
> cabal run dmq-node:dmq-tests
```

### CDDL

This project comes with a CDDL specification for the DMQ protocols
(`node-to-client` and `node-to-node`).  To check changes against the CDDL
specification, use:
```bash
cabal run dmq-node:dmq-cddl
```

# Contributing

The contributing guide is available [here][contributing-guide].
The style guide is available [here][style-guide].
The code of conduct is available [here][code-of-conduct].

# References

[cardano-node]:  https://github.com/intersectmbo/cardano-node
[contributing-guide]: ./CONTRIBUTING.md
[code-of-conduct]: ./CODE_OF_CONDUCT.md
[style-guide]: https://github.com/IntersectMBO/ouroboros-network/blob/main/docs/StyleGuide.md
[musl]: https://musl.libc.org/
[CIP#137]: https://cips.cardano.org/cip/CIP-0137
[CIP#155]: https://cips.cardano.org/cip/CIP-0155
[topology-file]: https://developers.cardano.org/docs/get-started/infrastructure/node/topology/
[defaultConfiguration]: http://intersectmbo.github.io/dmq-node/dmq-node/src/DMQ.Configuration.html#defaultConfiguration
[issue#6]: https://github.com/intersectMBO/dmq-node/issues/6
