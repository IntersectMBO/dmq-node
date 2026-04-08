# dmq-node changelog

<!-- scriv-insert-here -->

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
