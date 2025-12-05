# Decentralized Message Queue

TODO fix these links

[![mingw64](https://img.shields.io/github/actions/workflow/status/intersectmbo/ouroboros-network/build.yml?event=merge_group&label=mingw64&style=for-the-badge)](https://github.com/intersectmbo/ouroboros-network/actions/workflows/build.yml)
[![Nightly CI](https://img.shields.io/github/actions/workflow/status/intersectmbo/ouroboros-network/nightly.yml?branch=main&label=Nightly&style=for-the-badge)](https://github.com/intersectmbo/ouroboros-network/actions/workflows/nightly.yml)

The DMQ node allows for client peers to communicate efficiently by publishing
and consuming messages which are diffused over a P2P network to other nodes.

This repository provides the `dmq-node` executable to participate in the DMQ network.

TODO diagram
```mermaid
stateDiagram-v2
    dn: dmq-node
    tr: trace-dispatcher/iohk-monitoring-framework
    on: ouroboros-network
    oc: ouroboros-consensus
    cl: cardano-ledger
    dn --> tr
    dn --> on
```

# Instructions

TODO

# Contributing

The contributing guide is available [here][contributing-guide].
The style guide is available [here][style-guide].
The code of conduct is available [here][code-of-conduct].

# References

[cardano-node]:  https://github.com/intersectmbo/cardano-node
[contributing-guide]: ./CONTRIBUTING.md
[code-of-conduct]: ./CODE_OF_CONDUCT.md
[style-guide]: ./docs/StyleGuide.md
