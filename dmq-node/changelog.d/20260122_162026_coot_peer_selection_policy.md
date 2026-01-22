### Breaking

- `validateSig`: removed the hashing function for cold key from arguments, added required constraints ledger's `hashKey . VKey` usage instead

### Non-Breaking

- Added a lock to avoid race conditions between trace events.
- Improved peer selection policy.

