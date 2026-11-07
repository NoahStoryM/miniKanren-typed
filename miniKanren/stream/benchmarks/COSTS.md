# Residual stream overhead (2026-11-08)

This examines the final integrated runtime at
`e4262a4c3078777514a02f7f18479bc8eca519e6`; its stream code is unchanged from
the measured integration in FINAL.md and the final #16 extension. Production
runtime code and the extension branch are not modified by this experiment.

## Same-round diagnostic measurements

Racket 9.3 CS, Linux x86_64. TRS2 only: the same million-answer and 1000-query
seven-bit branching workloads as FINAL.md. Each backend/workload has three
independent processes, three samples per process, small/full warmups, and
three full collections before each sample. Compilation, loading, type checking,
and preview validation are excluded; CPU includes query-time GC. Backends use
the same explicit Data predicate. Table entries are medians of process medians.

| Variant | Answers CPU (ms) | Branching CPU (ms) |
| --- | ---: | ---: |
| Original checked / manual predicate | 228 | 83 |
| Original no-check / manual predicate | 258 | 94 |
| Final stream | 464 | 229 |
| Read first only once in each ready clause | 425 | 230 |
| Do not write single-step result caches | 437 | 224 |
| Omit single-step forcing guards | 478 | 194 |

All 108 samples have expected counts, and preview outputs agree across backends
and independent processes. Raw CPU/real/GC samples are preserved in
`final-cost-results-20261108.json`. These are a separate measurement round;
compare variants within this table rather than subtracting absolute times from
the earlier FINAL.md table.

The diagnostic changes apply to temporary copies only:

- stream-once retains the API and uses => to reuse the first peek's list,
  avoiding another traversal, classification, and values-list construction.
- stream-no-cache restores the thunk instead of retaining its result, skips
  the creator-state update and prefix compression. It retains native structs,
  thunks, reentrancy/result/cycle guards, and ordinary stream-force caching.
  It can change work when an input is reused; it does not remove the entire
  representation or memory cost associated with memoization.
- stream-no-guards removes only stream-force-once's reentrant sentinel write,
  result validation, and cache-cycle check. It retains memoization, compression,
  and the ordinary stream-force implementation and its guards. It deliberately
  changes invalid-input behavior and is not proposed as a public replacement.

These changes are not an additive cost decomposition. Cache removal can change
recomputation and object retention; changing code can affect compiler decisions
and GC. The driver source hash and measured commit are recorded in the raw file.

## What the measurements establish

Single-step cache removal improves answers by about 6% and branching by about
2% in this run. It leaves most of the gap to the original implementation.
It therefore does not support attributing the entire slowdown to the two
cache-field updates or prefix compression. It also does not establish that
memoization as a whole costs only these percentages.

Reading first only once improves answers by about 8%, but has no consistent
branching benefit: before/once process medians are 229/215, 236/230, 223/243 ms.
This combines reduced API calls and reduced list packaging, so it does not
isolate the cost of the list allocation alone.

Omitting the single-step guards improves branching by about 15%, with lower
times in all three process comparisons (229/183, 236/194, 223/217 ms). Yet
194 ms remains well above the checked original's 83 ms. The answers case has
no consistent guard-removal improvement. This run does not distinguish the
individual remaining guards. Earlier guard results predate the #18 identity
fast path and must not be reused as percentages for this final code.

For the full stream backend, answers has CPU/GC medians of 464/262 ms;
branching has 229/4 ms. GC accounts for a substantial part of the million-answer
runtime, but only about 2% of branching. GC time alone cannot explain the
branching slowdown. These timings do not directly count allocations or measure
peak memory.

## Remaining work visible in the source

The following comparison explains where extra work exists. It is source
analysis, not a measured percentage attribution or sampling profile.

| Operation | TRS2 pair/thunk representation | Final native stream representation |
| --- | --- | --- |
| Construct a ready node | cons of answer and rest | eager stream struct with first-forced marker, first value, and rest; constructor also supports multiple values and checks the tail stream |
| Construct a pause | thunk | thunk held in a mutable delayed-stream object with a creator/state field |
| Recognize and read an answer | pair? and car | stream-peek/first readiness and node checks, opaque-wrapper handling, cache-link traversal when needed; single cached values are exposed as a new one-element list in the source |
| Read rest | cdr | node/wrapper checks and cached-link traversal before reading the stored rest |
| Resume a pause | call the thunk | recognize/cache state, call the thunk, validate, check cycles, store result/state, and compress existing cached links |

The ports use stream-cons with both #:eager flags. They do not allocate a new
lazy head and lazy tail for every ready pair; the additional delayed-stream
objects occur at the original explicit suspension sites. Constructor lambdas
and multiple-values support appear in the source, but the compiler can eliminate
some wrappers; source steps should not be counted as guaranteed allocations
without further measurement.

TRS1 additionally matures a stream through stream-force in case∞, then uses
generic stream-empty? and first/rest queries to distinguish unit and choice.
Its path differs from TRS2, so this TRS2 diagnostic does not quantify TRS1's
individual costs.

The remaining overhead plausibly lies in the larger representation, extra
objects and retention, and repeated construction/state-query/forcing paths.
Their separate shares have not been measured. In particular, these data do
not establish that multiple-value support, generic protocol dispatch, struct
allocation, or wrapper checks individually dominate.

Source references: the final extension's
[primitive implementation](https://github.com/NoahStoryM/miniKanren-typed/blob/039e5fc556f578b583a4dd06b82c4757469bc1e3/private/racket/private/stream-cons.rkt)
and the integration's
[original TRS2](https://github.com/NoahStoryM/miniKanren-typed/blob/e4262a4c3078777514a02f7f18479bc8eca519e6/typed/miniKanren/trs2/main.rkt).

## Reproduce

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --editions trs2 --workloads answers branching \
  --processes 3 --rounds 3 \
  --variants stream stream-once stream-no-cache stream-no-guards \
  --output cost-local.json
```

No production source, package installation, or #16 branch changes are needed.
