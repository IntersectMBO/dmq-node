<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

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

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
