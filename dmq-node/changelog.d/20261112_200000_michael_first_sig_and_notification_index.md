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

- The first sig a node accepted was never diffused nor delivered over
  `LocalMsgNotification`: `Mempool.empty` gave it index -1, the same as
  `mempoolZeroIdx`. The mempool now starts at index 0.
- `LocalMsgNotification` sent messages again on the next request: the server
  kept the index of the first message of a reply, not of the last.
