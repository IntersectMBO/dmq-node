<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

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


<!--
### Patch

- A bullet item for the Patch category.

-->
