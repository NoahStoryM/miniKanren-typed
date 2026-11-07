# Reuse Kanren cache observations (2026-11-08)

Retained implementation commit:
`0dea0429e73fbe6aacacbccbd6a401a72b1eae4a`.
Only the Kanren stream ports change; private/racket/stream remains exactly the
#16 extension at `039e5fc556f578b583a4dd06b82c4757469bc1e3`.
#17 remains downstream and is not merged back into #16.

## Small caller changes

- microKanren mplus/bind and TRS2 append-inf, append-map-inf, take-inf, and once
  reuse stream-peek-first's result through cond =>. The cached values list is
  inspected once instead of querying the stream a second time.
- microKanren mplus/bind and TRS2 append-inf, append-map-inf, ifte, and once
  bind the initial stream-peek result and reuse it for empty/cache branches.
  These pure queries are not moved across goal execution or suspension forcing.
- TRS1 case∞ reads the stored rest once after checking for empty, then reuses it
  to distinguish unit/choice and bind the choice tail. The original tail object's
  identity is retained, including a suspension that has already cached empty.
- take-inf retains its zero-answer guard before any stream query; take and
  take-all retain their existing behavior. No suspension site, #:eager flag,
  goal algorithm, expected fixture answer, or stream-library API changes.

There is no new abstraction layer, specialized constructor, or alternate
stream representation. Additional whole-cache reuse in take-inf was left out
rather than introducing another conditional layer around its n=0 guard.

## Validation

The first-only candidate passed the full 2117 assertions (833 stream and 1284
Kanren). The retained more-reuse candidate passed all 1284 Kanren assertions,
including unchanged book fixtures, cold/warmed traces, infinite branches,
and untouched tails. The library is byte-identical to the tested 833-assertion
stream library. The retained TRS1/TRS2 source hashes match the measured
stream-more variant; microKanren uses the same tested candidate source.
No new microKanren timing claim is made here.

## Same-round comparison

Racket 9.3 CS, Linux x86_64. Three independent processes per backend/workload,
three measured samples per process, small/full warmups, and three collections
before each sample. CPU includes query-time GC; compilation, type checking,
loading, preview checking, and typed/untyped boundary contracts are excluded.
Every sample builds fresh queries; all backends use the same manual Data
predicate. The workloads match FINAL.md: a million answers, 5000 split queries,
and 1000 seven-bit branching queries.

CPU table entries are medians of three process medians, in milliseconds:

| Edition | Workload | Before | First once | More reuse (retained) | Change vs before |
| --- | --- | ---: | ---: | ---: | ---: |
| trs1 | answers | 471 | 472 | 385 | -18.3% |
| trs1 | splits | 109 | 97 | 109 | +0.0% |
| trs1 | branching | 179 | 181 | 173 | -3.4% |
| trs2 | answers | 540 | 466 | 471 | -12.8% |
| trs2 | splits | 119 | 114 | 103 | -13.4% |
| trs2 | branching | 246 | 240 | 241 | -2.0% |

First once is an identical-code control for TRS1: it has no repeated first
query in the same ready clause. Its 109 → 97 ms split variation therefore
cannot be attributed to an optimization. All process samples remain in the raw
report, rather than selecting the best run.

The retained TRS1 million-answer candidate is faster in all three process
comparisons: before/retained medians 439/385, 477/374, 471/385 ms. Its GC medians
fall from 161,185,167 to 99,98,98 ms. The source change can also affect compiler
liveness decisions; the measurements do not prove that the removed rest query
itself allocated that memory or count peak memory/allocations.

TRS2 first-only million-answer medians are 466,475,453 ms, versus baseline
541,540,519. The additional cache reuse gives 471,458,490: no further consistent
million-answer gain over first-only. However, it improves TRS2 splits in all
three comparisons with first-only (114/103,105/103,120/117 ms). Other selected
workloads are approximately unchanged or show small changes; this is not a
universal speedup.

For comparison with the original representations in the same round:

| Edition | Workload | Original checked/manual | Original no-check/manual | Retained stream |
| --- | --- | ---: | ---: | ---: |
| trs1 | answers | 147 | 104 | 385 |
| trs1 | splits | 73 | 74 | 109 |
| trs1 | branching | 75 | 72 | 173 |
| trs2 | answers | 257 | 291 | 471 |
| trs2 | splits | 73 | 79 | 103 |
| trs2 | branching | 87 | 94 | 241 |

All 270 measured samples have expected counts and matching preview outputs.
This is a new round; do not subtract its absolute timings from historical
FINAL.md or COSTS.md tables. It compares caller changes with all caching,
validation, reentrancy, and cycle protection intact.

## Exact candidate provenance and reproduction

The baseline is `5cd7c74e6020cf0d72f0c9fdceae92c60e53d858`. Both saved patches
in peek-reuse-candidates/ apply to that baseline:

- first-once.patch: first-value reuse only.
- more-reuse.patch: the full retained caller changes, including the first reuse.

The experiment ran before committing those candidates: report.commit records
the baseline HEAD, while native_sources records the actual source hash for each
edition/backend. stream-before is baseline; stream is first-only; stream-more is
the retained candidate. The driver hash is recorded too.

Create separate worktrees at the baseline, apply the two patches to the first
and more worktrees, and run the updated driver with those checkout paths:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --processes 3 --rounds 3 \
  --variants stream-before stream-first stream-more \
  --kanren-variant stream-before=/path/to/baseline-checkout \
  --kanren-variant stream-first=/path/to/first-only-checkout \
  --kanren-variant stream-more=/path/to/more-reuse-checkout \
  --output peek-reuse-local.json
```

stream-first in this command names the same source variant labelled stream in
the stored report. The driver copies the common stream library and each Kanren
source into isolated temporary modules. No package replacement or #16 changes
are required. Native source hashes allow checking that the variants match.
