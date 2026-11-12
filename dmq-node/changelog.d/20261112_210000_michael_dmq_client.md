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

- Added `dmq-client`, a node-to-client client: `submit` signs a message with
  a pool's KES key and submits it, `notify` prints received messages.
- `DMQ.NodeToClient` exports `localMsgSubmissionMiniProtocolNum`,
  `localMsgNotificationMiniProtocolNum` and `localMiniProtocolLimits`;
  `DMQ.Protocol.SigSubmission.Codec` exports `encodeSigPayload`.
