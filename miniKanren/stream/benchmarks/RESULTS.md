# Kanren runtime comparison, corrected (2026-11-07)

Measured implementation commit: `a3a775adc57ee5fef34b7d7cfa8fa3a55b420a21`.
Racket 9.3 CS, Linux x86_64. Raw samples, CPU/real/GC times, answer counts,
preview hashes, and source SHA256 hashes are in `results-20261107.json`.

## Correction: no-check changes generated predicates

The first TRS1/TRS2 comparison was confounded by `define-predicate` under
no-check. On Racket 9.3 CS it expands to a predicate accepting only #f, not
the requested Data type. `data?` therefore took different paths through ext-s.
Matching query answers did not detect this: the occurs-check fallback still
produces the same substitutions for valid terms. The original observations
are preserved in `results-20261107-superseded.json`, explicitly marked as
superseded. They do not isolate normal type-mode overhead.

`no-check-predicate-repro.rkt` reduces this behavior to an Integer predicate.
Both submodules contain `(define-predicate integer-value? Integer)` and
apply it to `#f` and `1`; only the languages differ:

```text
(checked (#f #t))
(no-check (#t #f))
```

Inspection of expansion shows that the no-check predicate obtains
`(flat-contract-predicate #f)`, while checked expansion replaces the lifted
placeholder with the generated contract. This is a concrete semantic
reproduction for the tested runtime, independent of the timing measurements.
The same no-check defect was already reported in
[typed-racket #316](https://github.com/racket/typed-racket/issues/316) on
2016-03-02 and remains open when checked on 2026-11-07. Its report also
identifies the unfilled #f contract placeholder. No upstream issue or comment
has been submitted by this work; a duplicate no-check report is unnecessary.

The stream implementations now spell out the same Data predicate explicitly,
retaining its predicate type annotation. There are 38 new assertions comparing
this predicate with the original checked predicate on literals, ground pairs,
variables, mixed pairs, and non-terms. Combined Kanren tests: 1284 assertions
(605 original fixtures and 679 direct checks); stream tests: 833 assertions.

## Corrected method

For TRS1/TRS2, four backends are measured:

- `typed`: the original checked source and its generated Data predicate.
- `typed-manual`: checked original with only Data's predicate written explicitly.
- `no-check`: original under no-check with that same explicit predicate.
- `stream`: stream/no-check implementation with the same explicit predicate.

microKanren has no generated Data predicate and needs only three backends.
The checked/manual and no-check/manual controls separate language mode from
the implementation of the runtime predicate. The driver verifies predicate
behavior before timing, in addition to matching answer previews and counts.

The benchmark relations and timer run inside each implementation module.
Compilation, type checking, module loading, and preview validation are excluded.
No typed/untyped boundary is crossed inside the timed region. Every backend/
workload has a fresh process, small and full-size warmups, and seven samples.
Three full collections precede each sample; CPU time includes GC during the
query. Every query and stream is newly constructed. Backend order rotates.

Workloads include construction, search, reification, answer-list allocation,
and counting:

- `answers`: 1,000,000 answers from a recursive relation returning `a`.
- `splits`: 5,000 queries enumerating the 13 splits of a 12-element list
  (65,000 answers).
- `branching`: 250 queries enumerating all 128 seven-bit lists (32,000 answers).

All 231 timed samples have the expected answer count; untimed previews agree.

## CPU medians (milliseconds)

micro's columns labelled manual simply mean no generated predicate is present.

| Edition | Workload | Original TR, generated predicate | TR, manual predicate | no-check, manual predicate | Stream, manual predicate |
| --- | --- | ---: | ---: | ---: | ---: |
| micro | answers | 318 | — | 368 | 1317 |
| micro | splits | 149 | — | 152 | 305 |
| micro | branching | 53 | — | 53 | 325 |
| trs1 | answers | 257 | 140 | 102 | 508 |
| trs1 | splits | 738 | 64 | 69 | 113 |
| trs1 | branching | 97 | 16 | 17 | 54 |
| trs2 | answers | 323 | 227 | 266 | 544 |
| trs2 | splits | 777 | 73 | 76 | 121 |
| trs2 | branching | 99 | 19 | 22 | 107 |

The large checked/no-check split-query difference mostly disappears when both
use the same manual predicate: TRS1 is 64 vs 69 ms, TRS2 is 73 vs 76 ms.
Keeping type checking enabled and changing only the predicate implementation
reduces TRS1 from 738 to 64 ms and TRS2 from 777 to 73 ms. This localizes the
large slowdown to generated-predicate code, not ordinary type annotations or
static checking time. Other workloads still differ in allocation and GC;
these few cases are not a universal performance ratio.

## Predicate-only reproduction

`predicate-cost.rkt` keeps both predicates and timers in one checked module,
with no exports or typed/untyped interaction. It checks a 12-element ground
list 100,000 times per sample, after compilation and warmup. CPU samples (ms):

```text
generated: 946 914 899 951 911 977 921   (median 921)
manual:      9   7   7   8   8   7   7   (median   7)
```

The predicates return the same answers, but their generated code has very
different performance here. This is a reduced performance investigation, not
proof of a compiler bug or a guarantee about other predicates. `define-predicate`
explicitly asks for a runtime check even inside a typed module.


## Scaling and upstream triage

`predicate-scaling.rkt` constructs the inputs at run time and measures 10,000
calls at each list length, three times. CPU medians (ms):

| List length | Generated Data | Generated narrow type | Manual Data |
| ---: | ---: | ---: | ---: |
| 0 | 1 | 1 | 0 |
| 1 | 9 | 4 | 0 |
| 2 | 15 | 7 | 0 |
| 4 | 30 | 14 | 0 |
| 8 | 58 | 25 | 0 |
| 16 | 120 | 49 | 1 |
| 32 | 239 | 96 | 3 |
| 64 | 504 | 218 | 4 |

Raw samples are in `predicate-scaling-results-20261107.json`. Narrow means
`(U Symbol Null (Pair Narrow Narrow))`. Small manual measurements fall below
whole-millisecond timer resolution; use the longer predicate-cost experiment
for that ratio. The 16/32/64 Data cases grow approximately linearly in this
range, not quadratically or exponentially. This is empirical evidence over
these inputs, not a proof about all recursive types or input graphs.

Expanded Racket 9.3 code implements Data using flat recursive contracts and
an or/c-style union of predicates. The numeric portion of the mixed union is
expanded into many numeric subtypes instead of a single number? test. That is
one source of avoidable-looking work, but it is not the whole gap: the narrow
recursive type also has substantial cost. The recursive-contract first-order
path also goes through generic contract machinery. No profile assigns an
exact fraction of runtime to either mechanism.

A standalone Number-generated predicate is close to number? in a separate
1,000,000-call control: medians 3 vs 2 ms. The observation is therefore about
these recursive union predicates, not every define-predicate expansion.

The documentation promises a predicate for the specified type, not equality
with handwritten-predicate performance. The measured gap is explainable by
the current implementation, but does not appear to be a semantic requirement.
It merits a separate performance/optimization issue with the fully typed,
no-boundary reproductions. The performance searches did not find a clear
matching issue. That report should not be conflated with known no-check
correctness issue #316. Racket's development version has not been tested here.

## Scheduling and stream costs

The implementations preserve the tested cold-run answer and pause order.
TRS1 retains the empty/unit/choice distinction and both merge/bind strategies;
TRS2 retains append-inf, append-map-inf, take-inf, ifte, and once. Cache-link
branches introduce no new pause. Tests cover generated stream expressions,
cooperative infinite branches, consecutive pauses, ifte/once, untouched tails,
and warmed-cache behavior. Memoization still changes reuse/effect semantics,
as described in the parent README.

The stream version remains slower than the original/no-check version on these
workloads. It replaces compact pairs/thunks with native stream structures,
cache maintenance, guard checks, and peek operations. In particular,
stream-peek-first allocates a values list and some call sites currently query
it twice per answer. These are potential optimization targets, not a profile
attributing specific fractions of runtime to each cost.

This differs from the earlier native-stream extension benchmark, which kept
the native representation constant and compared it before/after API additions.

## Reproduction

With dependencies available, from the repository root:

```sh
PLTCOLLECTS="$PWD:" python miniKanren/stream/benchmarks/compare.py \
  "$(command -v racket)" --rounds 7 \
  --output miniKanren/stream/benchmarks/results-local.json

raco make miniKanren/stream/benchmarks/predicate-cost.rkt
racket miniKanren/stream/benchmarks/predicate-cost.rkt
racket miniKanren/stream/benchmarks/no-check-predicate-repro.rkt
raco make miniKanren/stream/benchmarks/predicate-scaling.rkt
racket miniKanren/stream/benchmarks/predicate-scaling.rkt
```

Temporary copies are used for the comparison; the original implementations
remain unchanged.
