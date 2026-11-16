<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

<!--
### Breaking

- A bullet item for the Breaking category.

-->
<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
### Patch

- `InvalidOCertCounter` no longer disconnects a node-to-node peer: whether it
  fails depends on the order in which a node received signatures signed with
  different operational certificates, so the peer might have accepted the
  signature legitimately.  The counter is now checked after the cryptographic
  checks, so a forged signature is still rejected with an error which
  disconnects the peer.
