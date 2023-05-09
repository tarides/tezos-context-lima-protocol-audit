# Inputs

This directory contains inputs used for the various tests and audits:

- [store-level-2981990](./store-level-2981990): The Tezos Octez store at level 2_981_990. This corresponds to the start of the Lima protocol, which was activated at level 2_981_889.
- [actions.trace](./actions.trace): A `tezos-context` trace that records 100_000 blocks starting at level 2_981_990. See also the [tezos-context-trace repository](https://github.com/tarides/tezos-context-trace).
- [store-level-3081990](./store-level-2981990): The Tezos Octez store at level 3_081_990. This is the `store-level-2981990` after the `action.trace` is applied using Tezos Octez v16.0 and Irmin v3.7.1.

See the [irmin-tezos-benchmarking repository](https://github.com/tarides/irmin-tezos-benchmarking) for more information on the provenance.
