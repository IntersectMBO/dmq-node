<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Using `KeepAliveRegistry` instead of `FetchClientRegistry` introduced in
  a recent `ouroboros-network` PR. The `fetchClientRegistry` field of
  `NodeKernel` was replaced with `keepAliveRegistry` field.

### Non-Breaking

- Added `PrettyShow` instances for
  - `NodeToNodeVersion`
  - `NodeToNodeVersionData`
  - `NodeToClientVersion`
  - `NodeToClientVersionData`
- Signature validation changed, we're no longer using the `mark set`, pools
  with no stake will be able to mint signatures as long as they have non zero
  stake in the `set set`.


<!--
### Patch

- A bullet item for the Patch category.

-->
