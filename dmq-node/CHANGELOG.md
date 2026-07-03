# dmq-node changelog

<!-- scriv-insert-here -->

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 -- 2026-07-03

### Breaking

- Using `KeepAliveRegistry` instead of `FetchClientRegistry` introduced in
  a recent `ouroboros-network` PR. The `fetchClientRegistry` field of
  `NodeKernel` was replaced with `keepAliveRegistry` field.

- Lower `minSigBodySize` to `90` bytes.

### Non-Breaking

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
