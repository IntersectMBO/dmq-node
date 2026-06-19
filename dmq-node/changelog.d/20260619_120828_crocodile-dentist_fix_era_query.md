<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

<!--
### Breaking

- A bullet item for the Breaking category.

-->
### Non-Breaking

- Fixes local state query application to correctly query the cardano-node's
  ledger era and re-uses that information for the relevant queries. Previous
  implementation was hardcoded to Conway era, which would cause a crash on
  a transition.

<!--
### Patch

- A bullet item for the Patch category.

-->
