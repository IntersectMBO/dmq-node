### Breaking

- Added `LocalMsgNotificationServerTracer` option (disabled by default)
- `Sig`: improved instances `ToJSON` and `Show`
- Refactored `validateSig`

### Non-Breaking

- By default enabled the folling traces:
  - `MuxTracer`
  - `TraceChurnCounters`
  - `LocalCOnnectionManagerTracer`
  - `SigSubmissionLogicTracer`
- Removed pre-commit-hook in nix shell
- Added negative test cases from `prop_validateSig` QuickCheck property
