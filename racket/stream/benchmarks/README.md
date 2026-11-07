# Two-state native-stream experiment (Racket 9.3 CS)

Entry point: `(require racket/stream/two-state)`.

This is an isolated primitive fork for studying extensions to Racket itself.
It does not install a patch into the running Racket distribution or replace
`racket/stream/search`. It copies the Racket 9.3 primitive source (including its
license) to `../private/two-state-cons.rkt`; the facade reuses stock generic
consumers and derived operations. Racket's stock empty-stream is recognized.

## Change

Keep both original structure layouts and the original pending/completed state
encoding. A completed delayed node may now cache another delayed node. The only
change inside the old `stream-force` body is: when a completed node's cached
result is another native delayed node, continue the existing loop and include
that node in the existing path-compression list. Otherwise use the old code.
Constructors and head forcing are unchanged. No new fields, forwarding tags,
promises, hash tables, or permanent history records are added.

- `stream-inspect`: non-forcing observation. Returns `empty`, `ready`,
  `head-lazy`, or `lazy`. `unknown` covers unsupported stream implementations;
  `blocked` reflects the existing running/broken guard, which does not
  distinguish running from failed evaluation. Already-cached links are followed.
- `stream-force-one`: follows cached links, evaluates at most one pending native
  stream thunk, checks that its result is a stream, caches the immediate result,
  and returns it. It does not force a cons node's head. A new cached edge is
  checked for cycles without forcing its target; failures retain the old guard.

Ordinary full forcing still compresses history. This API makes no promise that
past delay boundaries remain observable after evaluation. Ordinary Racket
stream-cons accepts one head expression and one tail expression; stream-lazy
accepts one stream expression (use begin for multiple effects).

The observer and one-step operation follow cached chains without compressing
them. Advance a cursor using the returned stream when stepping. Repeatedly
starting from an old root can repeatedly traverse a growing cached chain and
become quadratic; ordinary full force compresses that chain. The new-edge cycle
check can also traverse an already-cached target chain. These are costs of the
new operations; no global cycle table was added to the stock forcing path.

## Measurements

From repository root, compile first, then run sequentially:

```sh
raco make racket/stream/benchmarks/main.rkt racket/stream/benchmarks/steps.rkt
python racket/stream/benchmarks/run.py /path/to/racket
racket racket/stream/benchmarks/steps.rkt
raco test racket/stream/tests/two-state.rkt
```

Each stock/fork workload uses a fresh process, small and full warmups, seven
samples and full GC between samples. Backend order alternates between cases.
Reported numbers are CPU-time medians, include GC, exclude module loading and
compilation. This is a shared x86_64 environment; small percentage differences
are not stable estimates or confidence bounds. JSON includes raw time samples,
GC times, checksums, version, base commit, and both primitive source hashes.

| Workload | Size | Stock ms | Two-state ms | Ratio |
| --- | ---: | ---: | ---: | ---: |
| Eager construction + traversal | 1,000,000 | 146 | 151 | 1.034 |
| Lazy tail | 1,000,000 | 269 | 261 | 0.970 |
| Lazy head + tail | 1,000,000 | 265 | 280 | 1.057 |
| Nested stream-lazy chain | 2,000,000 | 196 | 196 | 1.000 |
| Repeated cached-head reads | 5,000,000 | 367 | 382 | 1.041 |

After full GC, retaining the consumed source gives approximately zero retained
heap for compressed delay chains (100k and 300k), and 19,186,560 bytes for a
300k lazy-tail stream, identical in both variants. Tiny negative deltas are
measurement/baseline noise, not negative allocation. This measures retained
managed heap, not peak memory or total allocation.

For a 300k delay chain, advancing a cursor one step at a time and then fully
forcing its original root takes a median 15 ms. With the root retained, the
stepped chain occupies 9,597,544 bytes (~9.15 MiB) before full forcing and
approximately zero afterward. This shows that the intermediate cache chain is
reclaimable through ordinary path compression; it does not show stepping to be
faster than full forcing (different workloads/sizes).

The 1,539 passing checks cover single steps, cache reuse, stock-consumer
interoperability, lazy heads/tails, multiple values, errors, cycles, and 500
seeded finite expressions compared with stock streams. This is a layer-2
experiment; it does not yet evaluate microKanren scheduling on the new API.
