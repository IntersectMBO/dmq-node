# dmq-node changelog

<!-- scriv-insert-here -->

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 -- 2026-11-27

### Breaking

- Signatures issued by the same pool are rate limited by their `sigExpiresAt`
  rather than by the time they were received: `sigExpiresAt` of any two
  signatures of a pool must be at least `minSigDelay` apart
  (`DMQ.Protocol.SigSubmission.SigSpacing`).  A burst of well-spaced signatures
  passes the spacing check in any order if none of its signed expiries conflicts
  with the receiver's history for that pool.  A node can still reject a
  signature whose OCert counter is below one it already accepted.
  Previously a burst of signatures which accumulated in a node's mempool was
  rejected with `SigTooFrequent` by its peers, which disconnected them
  (issue #82).
- `NodeToNodeV_3` supports sending these bursts over `SigSubmissionV2`.
  On V1 and V2 connections, the sender waits `minSigDelay` after the peer
  acknowledges a signature ID before announcing another from the same pool.
  Upgraded nodes apply the same signature validation rules on every connection.
- `SigTooFrequent` is checked after the cryptographic checks, and it no
  longer disconnects a node-to-node peer: the pool might have sent conflicting
  signatures to different nodes, which is not the peer's fault.
- `PoolValidationCtx`: removed `vctxNow`, `vctxLastSigByPoolId` was replaced
  with `vctxSigSpacing`; `withPoolValidationCtx` takes only `UTCTime`.
- `NodeKernel`: `lastSigByPoolIdVar` was replaced with `sigSpacingVar`.
- `DMQ.Mempool` re-exports `getWriter` from `ouroboros-network`.

### Non-Breaking

- Define directional mux ingress limits for signature submission and peer sharing in DMQ's policy.
  Signature replies use the larger queue on the V1 responder and the V2 initiator.
- Trace node-wide mux counters under `Net.Mux.Counters` at the diffusion default interval.
- LedgerPeers are now enabled by default.

### Patch

- Updated `ouroboros-consensus` to version `5.1.0.1`.

- Updated `ouroboros-consensus` to version `5.2`, `ouroboros-network` to version `2.0` and `network-mux` to version `1.0`.

- The first sig a node accepted was never diffused nor delivered over
  `LocalMsgNotification`: `Mempool.empty` gave it index -1, the same as
  `mempoolZeroIdx`. The mempool now starts at index 0.
- `LocalMsgNotification` sent messages again on the next request: the server
  kept the index of the first message of a reply, not of the last.

- `InvalidOCertCounter` no longer disconnects a node-to-node peer: whether it
  fails depends on the order in which a node received signatures signed with
  different operational certificates, so the peer might have accepted the
  signature legitimately.  The counter is now checked after the cryptographic
  checks, so a forged signature is still rejected with an error which
  disconnects the peer.

- Update to `ouroboros-network-2.1.0.0`, `cardano-diffusion-1.4.0.0`,
  `network-mux-1.1.0.0` and the compatible `ouroboros-consensus-5.4.0.1` revision.

<a id='changelog-0.7.2.1'></a>
## 0.7.2.1 -- 2026-10-12

### Patch

- Updated `ouroboros-consensus` to version `5.1.0.1`.

<a id='changelog-0.7.3.0'></a>
## 0.7.2.0 -- 2026-10-02

<a id='changelog-0.7.1.0'></a>
## 0.7.1.0 -- 2026-09-01

### Non-Breaking

- Bump `ouroboros-consensus` to `v4.1`.

- Build natively on `aarch64-linux`, included the build artifact it in a release.

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 -- 2026-07-31

### Breaking

- Added PeerSelectionPolicy's rng to `NodeKernel`.  This allowed to create the
  policy in `diffusionApplications` rather than in the `main` module.

- Using `tx-submission` undecision logic:
  - `NodeKernel` data type changed to adapt to the new tx-submission logic
  - `SigMempoolResult` is provided by `DMQ.Diffusion.PeerSelection.PeerMetric` module.
  - `sigSubmissionLogicPeerTracer` was removed from `DMQTracers`
  - `sigDecisionPolicy :: TxDecisionPolicy` was adapted to the new tx-submission logic

- Using `KeepAliveRegistry` instead of `FetchClientRegistry` introduced in
  a recent `ouroboros-network` PR. The `fetchClientRegistry` field of
  `NodeKernel` was replaced with `keepAliveRegistry` field.

- Lower `minSigBodySize` to `90` bytes.

### Non-Breaking

- Peer sharing delays and timeouts in `PeerSelectionPolicy`
- Explicit cborg error in local-msg-notification protocol
- Added tracers for NtC connection with `cardano-node`:
  - hanshake mini-protocol tracer
  - mux tracer
  - mux channel tracer
  - mux bearer tracer

- Integrated with `trace-dispatcher ^>= 2.13.0` and `contra-tracer >= 0.2.1.0`

- Added `PrettyShow` instances for
  - `NodeToNodeVersion`
  - `NodeToNodeVersionData`
  - `NodeToClientVersion`
  - `NodeToClientVersionData`
- Signature validation changed, we're no longer using the `mark set`, pools
  with no stake will be able to mint signatures as long as they have non zero
  stake in the `set set`.

- Fixes local state query application to correctly query the cardano-node's
  ledger era and re-uses that information for the relevant queries. Previous
  implementation was hardcoded to Conway era, which would cause a crash on
  a transition.

- Bugfix: don't set block point slot to maxBound for big ledger peers in local state query client

- Added `--min-sig-delay` internal flag.  One cannot use it on the mainnet.

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 -- 2026-06-04

### Breaking

- Default network magic used by dmq-node is the mainnet network magic for mithril network.
- Tighter sig-submission codec size limits (derived from `maxSigsInflight` and `maxSigSize`).
- Using `KeepAliveRegistry` instead of `FetchClientRegistry` introduced in
  a recent `ouroboros-network` PR. The `fetchClientRegistry` field of
  `NodeKernel` was replaced with `keepAliveRegistry` field.
- Using set snapshot in the pool eligibility (signature validation).
- Added `dmqcShelleyGenesisFile` and `dmqcShelleyGenesisHash` fields to
  `Configuration`.  The default value are `genesis-shelley.json` and
  `Nothing` (e.g. hash is not verified against expected value).
- Signature validation is using maximum KES evolutions from the genesis file.
- Signature validation changed, we're no longer using the `mark set`, pools
  with no stake will be able to mint signatures as long as they have non zero
  stake in the `set set`.

### Non-Breaking

- Added `PrettyShow` instances for
  - `NodeToNodeVersion`
  - `NodeToNodeVersionData`
  - `NodeToClientVersion`
  - `NodeToClientVersionData`

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 -- 2026-05-19

### Breaking

- Using `Hash Blake2b_256` hashing algorithm for computing `SigId`.
- Changed encoding of `Sig` according to: https://github.com/cardano-foundation/CIPs/pull/1185.
- Removed `TraceLocalMsgSubmission` `msg` parameter, since it was unused.
- Removed `ToJSON (TraceLocalMsgSubmission msg)` instance.
- Removed `ToJSON (AnyMessage SigSubmissionV2)` instance.

### Non-Breaking

- Added `DMQ.Diffusion.PeerSelection.PeerMetric`: peer-scoring metric that
  tracks how promptly each peer announces signatures, used by peer selection
  to prefer well-performing peers.
- Wired the metric into the sig-submission inbound client.

- Validation of `SigId`s is implemented using `Hash Blake2b_256` hashing
  algorithm.
- Implemented `DMaximum` verbosity for the following tracers:
  - `AnyMessage (LocalMsgNotification (Sig crypto))`
  - `AnyMessage (SigSubmissionV2 SigId (Sig crypto))`
  - `AnyMessage (TxSubmission2 txid tx)`
  - `TraceSigSubmissionOutbound SigId (Sig crypto)`
     - the `TraceSigSubmissionOutboundSendMsgReplySigs` logs `sigs` rather than
       `sigids` with different fields depending on the verbosity level.
  in `DMaximum` tracing, all fields of `Sig crypto` are logged.
- Logging of `AnyMessage (SigSubmissionV2 SigId (Sig crypto))` changed syntax:
  - log `sigids` of `MsgReplySigIds`
  - log `sigids` of `MsgRequestSigs` using JSON syntax

<a id='changelog-0.4.2.0'></a>
## 0.4.2.0 -- 2026-04-07

### Non-Breaking

- Changed default prefix for prometheus counters to `dmq-node_` (previously no
  prefix was added).  The prefix can be changed using
  `TraceOptionMetricsPrefix` option in a configuration file.

<a id='changelog-0.4.1.0'></a>
## 0.4.1.0 -- 2026-04-01

### Non-Breaking

- Default options for tracing configuration:
  - default severity `Info`
  - default backend `Stdout MachineFormat`
  - no prometheus
- Fixed `--version` flag

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 -- 2026-03-30

### Breaking

- Integration with `trace-dispatcher`.  Removed tracing configuration options
  from `Configuration`, `trace-dispatcher` configuration is used instead.
- Added EKG counters and a prometheus server.

### Non-Breaking

- Replaced `NoExtraPeers`, `NoExtraState`, `NoExtraDebugState`, `NoExtraFlags` with types from `ouroboros-network`.
- Removed unused types `NoExtraTracer`, `NoExtraCounters`.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 -- 2026-03-23

### Breaking

- Using `KESPeriod` from `Cardano.Crypto.KES` instead of `SigKESPeriod`
  newtype.  `KESPeriod` is used by `SigRaw` data type.
- `SigKESSignature` holds `SigKES (KES crypto)` instead of a `ByteString`.
- `SigColdKey` holds `VerKeyDSIGN` instead of a `ByteString`.
- `ntnApps` constraints changed in order to use `sigValidate` function.

- Removed KES evolution configuration & genesis file from the DMQ configuration.

- Added `LocalMsgNotificationServerTracer` option (disabled by default)
- `Sig`: improved instances `ToJSON` and `Show`
- Refactored `validateSig`

- `validateSig`: removed the hashing function for cold key from arguments, added required constraints ledger's `hashKey . VKey` usage instead

- Have the cardano-network-magic cli option set the correct magic field
- Add a dmq-network-magic cli option

- Implemented `SigSubmissionV2` protocol
- Added tests for the protocol codec
- Added support for both V1 and V2 signature submission protocols in the node

- Added `dmqLedgerPeers` option (off by default), which enabled ledger peers
  obtained through Cardano `node-to-client` protocol.

### Non-Breaking

- Dependencies updated to `ouroboros-network` and `cardano-diffusion`.

- `Sig` codec decodes KES signatures, and the cold key.
- Added `DMQ.SigSubmission.Type.validateSig` and `SigValidationError`.

- Update dependencies.

- By default enabled the folling traces:
  - `MuxTracer`
  - `TraceChurnCounters`
  - `LocalCOnnectionManagerTracer`
  - `SigSubmissionLogicTracer`
- Removed pre-commit-hook in nix shell
- Added negative test cases from `prop_validateSig` QuickCheck property

- improvements to local state query client tracing

- For pre-release development and testing purposes, use volatile tip
  in local state query client.

- Added a lock to avoid race conditions between trace events.
- Improved peer selection policy.

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 -- 2025-09-29

### Breaking

- Added a way to configure a unix socket for node to client communication.  It
  can either be specified in the configuration file or a the command line switch
  `--local-address`.
<!-- scriv-end-here -->

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
