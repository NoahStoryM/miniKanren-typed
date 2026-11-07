# Final integrated stream comparison (2026-11-08)

Measured Kanren integration commit:
`c1f19621eefe15e7f98cecfffa4c59ab7b7ce657`.
Its stream library is exactly the extension branch at
`039e5fc556f578b583a4dd06b82c4757469bc1e3`, including merged PRs #18 and #19
and the final direct-recursive first/rest peeks. No stream-library differences
remain between the integration and extension branches.

## Branch boundary

[PR #16](https://github.com/NoahStoryM/miniKanren-typed/pull/16) remains the
extension-only implementation reference, relative to the copied Racket stream
baseline. [PR #17](https://github.com/NoahStoryM/miniKanren-typed/pull/17) is
the downstream Kanren integration and evidence branch. The extension branch
was merged into the integration branch only; Kanren implementations, benchmarks,
and experiment records were not merged back into #16. Its
[Files changed](https://github.com/NoahStoryM/miniKanren-typed/pull/16/files)
can therefore remain the focused implementation reference for an upstream
feature request.

## Validation and method

The final integrated tree passes all 2117 assertions: 833 stream assertions
and 1284 Kanren assertions, including both editions' unchanged book fixtures,
cache/error cases, and scheduling traces.

Racket 9.3 CS, Linux x86_64. The three backends are:

- `typed-manual`: original pair/thunk implementation with type checking,
  replacing only the Data predicate with the explicit handwritten predicate.
- `no-check`: the same pair/thunk implementation and handwritten predicate,
  using typed/racket/no-check.
- `stream`: the integrated private/racket/stream implementation under no-check,
  with that same handwritten predicate.

The generated-predicate backend is deliberately omitted from this comparison.
No diagnostic ablation or candidate-library substitution is selected: stream
uses the final checked-in library. The generic warning in the ablation driver's
report describes its optional diagnostic modes, which are not selected here.

Each backend/workload runs in three independent processes, each with five
measured samples. Every process receives small and full-size warmups; three
full collections precede each sample. Backend order rotates across workloads
and process repetitions. Timers execute inside each implementation module:
compilation, type checking, loading, preview validation, and typed/untyped
module-boundary contracts are outside the timed region. CPU includes query-time
GC. Every sample constructs fresh queries and streams, without reusing warmed
results.

The table uses the median of the three process medians. Raw per-process
medians, all samples, CPU/real/GC times, preview hashes, answer counts, driver
hash, and Kanren source hashes are in `final-results-20261108.json`.

- answers: 1,000,000 answers from a recursive relation returning a.
- splits: 5,000 queries, each enumerating 13 splits of a 12-element list
  (65,000 answers per sample).
- branching: 1,000 queries, each enumerating all 128 seven-bit lists
  (128,000 answers per sample).

The earlier RESULTS.md branching workload used 250 queries. Its absolute
branching times should not be compared directly with this table.

## Final CPU medians (milliseconds)

| Edition | Workload | Original TR / manual | Original no-check / manual | Final stream | Stream / TR | Stream / no-check |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| trs1 | answers | 147 | 99 | 439 | 2.99× | 4.43× |
| trs1 | splits | 71 | 67 | 93 | 1.31× | 1.39× |
| trs1 | branching | 72 | 79 | 181 | 2.51× | 2.29× |
| trs2 | answers | 246 | 268 | 528 | 2.15× | 1.97× |
| trs2 | splits | 79 | 76 | 113 | 1.43× | 1.49× |
| trs2 | branching | 87 | 82 | 247 | 2.84× | 3.01× |

All 270 measured samples have the expected answer counts; preview outputs
agree across all backends and independent process repetitions. These are the
final integrated code's measurements, including the direct-recursive peeks.
The stream/TR ratio ranges from 1.31× to 2.99× in these six workloads.

For trs1, stream query-time GC medians are 161 ms in answers and 2 ms in branching.
For trs2, stream query-time GC medians are 300 ms in answers and 5 ms in branching.
GC is substantial in the million-answer workloads, but accounts for only a
small part of the branching CPU times. This does not decompose the remaining
cost into caching, wrappers, predicates, or dispatch.


## Scope of the result

These are end-to-end search workloads, including construction, unification,
reification, answer-list allocation, and counting. Their ratios do not isolate
a fixed overhead for memoization or any single stream operation. Shared-machine
and GC variation remain visible in the raw process results; these are selected
workloads, not a universal speed ratio.

The functional tests compare cold answer/pause traces and also test cache reuse.
Native streams memoize; the original thunks do not. Reusing warmed inputs can
change the interleaving of a newly constructed merge, as documented in the
implementation README. Matching these fresh benchmarks does not assert identical
observable behavior for arbitrary reused or effectful streams.

This six-case comparison covers TRS1 and TRS2. microKanren is included in the
functional regression suite, but no new microKanren timing is claimed here.
The earlier predicate and dispatch experiments remain as historical reports
for their recorded revisions.

## Reproduce

From the measured integration commit, with the package dependencies available:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --variants stream --processes 3 --rounds 5 \
  --output final-local.json

PLTCOLLECTS="$PWD:" raco test \
  private/racket/tests/compatibility.rkt \
  private/racket/tests/extensions.rkt \
  microKanren/stream/tests miniKanren/stream/trs1/tests miniKanren/stream/trs2/tests
```

Both commands use the integrated library directly. No candidate collection
override, stream-source replacement, or ablation is needed.

For a follow-up diagnostic of residual overhead on the same runtime, see
[COSTS.md](COSTS.md). Its temporary variants and same-round timings are separate
from the final production comparison above.

Subsequent caller-side query reuse, with the stream library unchanged, is
recorded in [PEEK-REUSE.md](PEEK-REUSE.md). That report contains the latest
Kanren source and same-round measurements.
