# Decentralized Message Queue

The DMQ node allows for client peers to communicate efficiently by publishing
and consuming messages which are diffused over a P2P network to other nodes.

This repository provides the `dmq-node` executable to participate in the DMQ
network.

The `dmq-node` is developed with respect to the [CIP#0137].

__NOTE__: This is still an early version of DMQ node, which comes with some
quirks:

* issue#6 - no support for ledger peers, which requires setting up static peers
            using local roots.  We are aiming to add this feature for the
            `cardano-node-10.7` release.
* issue#13 - using `TxSubmission` mini-protocol for which roles are swapped,
             e.g. server requests data, client servers the data.  This makes
             configuration awekward, since your local roots specify who will get data from
             you, rather than who you get data from.  We are working on a new
             mini-protocol to address this.

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

## Developing with Nix

To enter a development shell with all dependencies available, use:
```bash
> nix develop
```

## Testing the project

To run the test suite, one can use:

``` bash
> cabal test all
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
[style-guide]: ./docs/StyleGuide.md
[musl]: https://musl.libc.org/
[CIP#0137]: https://cips.cardano.org/cip/CIP-0137
