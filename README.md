# Decentralized Message Queue

The DMQ node allows for client peers to communicate efficiently by publishing
and consuming messages which are diffused over a P2P network to other nodes.

This repository provides the `dmq-node` executable to participate in the DMQ network.

# Instructions

## Building the project

We use cabal to build our project, potentially inside a Nix shell (nix develop or nix-shell). It should suffice with:
```bash
> cabal build dmq-node
```

The executable can be run with:
```bash
> cabal run dmq-node
```

## Testing the project

To run the test suite, one can use:

``` bash
> cabal test all
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
