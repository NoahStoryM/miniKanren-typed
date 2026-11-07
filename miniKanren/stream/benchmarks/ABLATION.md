# Native stream overhead diagnostics (2026-11-07)

Racket 9.3 CS, Linux x86_64. Production implementation commit:
`9b69a0a3918d087f45199e0c34bbe7d0fe73cd37` (runtime sources unchanged since
the Data-predicate correction). These experiments change temporary copies,
not production modules or the API.

All backends use the same explicit Data predicate. The primary original
comparison uses Typed Racket with checking enabled (`typed-manual`); the
stream version uses no-check. A pair/thunk no-check control is also recorded
in the raw data. Compilation, type checking, loading, and module-boundary
contracts are outside the timed region. Fresh processes, warmups, seven
samples per case, and three full collections before each sample follow the
main comparison driver. CPU times include query-time GC.

## Main comparison

CPU medians, milliseconds:

| Edition | Workload | Original TR, manual predicate | Stream/no-check | Stream / TR |
| --- | --- | ---: | ---: | ---: |
| trs1 | answers | 147 | 581 | 3.95× |
| trs1 | splits | 70 | 107 | 1.53× |
| trs1 | branching | 70 | 212 | 3.03× |
| trs2 | answers | 239 | 545 | 2.28× |
| trs2 | splits | 75 | 132 | 1.76× |
| trs2 | branching | 87 | 442 | 5.08× |

- answers: 1,000,000 answers from a recursive relation returning a.
- splits: 5,000 fresh queries enumerating 13 splits (65,000 answers).
- branching: 1,000 fresh queries enumerating 128 seven-bit lists (128,000 answers).

The branching count is four times the earlier main comparison; do not compare
its absolute times directly with that report. Shared-machine variability is
also visible between runs. These are selected workloads, not a universal ratio.

## Diagnostic ablations

| Edition | Workload | Stream | Query first once | No step cache | No forcing guards |
| --- | --- | ---: | ---: | ---: | ---: |
| trs1 | answers | 581 | 537 | 487 | 507 |
| trs1 | splits | 107 | 107 | 108 | 99 |
| trs1 | branching | 212 | 215 | 232 | 186 |
| trs2 | answers | 545 | 530 | 490 | 512 |
| trs2 | splits | 132 | 131 | 142 | 119 |
| trs2 | branching | 442 | 451 | 415 | 228 |

- Query first once rewrites ready clauses in TRS2 to use => and reuse the
  result of stream-peek-first. TRS1 has no matching repeated query to rewrite,
  so that variant is an identical-code control for it. Its variation helps
  illustrate noise, especially in GC-heavy answers measurements.
- No step cache retains forcing guards but restores the original thunk
  instead of storing its result. It skips the forced-state update and prefix
  compression. The delayed structure and thunk remain, as does full
  stream-force's existing caching. This is not a measurement of removing
  every allocation or cost associated with memoization.
- No forcing guards keeps caching but removes reentrant-thunk protection,
  validation that a forced result is a stream, and cycle detection from
  stream-force-once. The individual guard variants below remove one at a time.

The no-cache variant deliberately changes memoization and may change the work
performed when an input is reused. Guard variants deliberately change invalid-
input behavior. All selected cold queries have matching preview answers and
expected answer counts, but these are not proposed safe public replacements
or an additive decomposition of runtime. No production guard was removed.

Eliminating repeated first queries showed no consistent material improvement
in this run. Single-step cache removal did not eliminate the overall gap;
it sometimes improved GC-heavy answers cases and sometimes made a case slower.
Neither result proves a fixed percentage cost for caching or peeking.

## TRS2 branching: isolate forcing guards

A separate run uses the same 1,000-query branching workload:

| Variant | CPU median (ms) | GC median (ms) |
| --- | ---: | ---: |
| typed-manual | 86 | 1 |
| no-check | 89 | 1 |
| stream | 449 | 8 |
| stream-no-cycle | 243 | 4 |
| stream-no-result | 418 | 9 |
| stream-no-reentrance | 419 | 7 |
| stream-no-guards | 215 | 4 |

The largest observed individual change is removal of the cache-cycle guard:
449 to 243 ms while retaining memoization. Removing result validation or
reentrant protection individually has a much smaller effect in this case.
The cycle guard includes impersonator-of? and traversal of already-cached
links; this experiment does not separate the two. The current extension's
cycle check is therefore a significant optimization target for this workload,
not evidence that storing stream results inherently accounts for all overhead.
The ordinary Racket stream-force implementation is not modified by these
ablations.

There is still substantial time after removing all these guards. Other extra
work visible in the source includes native eager/delayed structures, closures
inside delay objects, first/rest/whole-stream state queries, result-values
packaging, and full-force normalization in TRS1 case∞. Those remaining costs
have not been individually quantified. GC accounts for only 8 ms of the
449-ms branching measurement, so GC alone does not explain that case. In the
answers workload, allocations, retention, and GC are much more important.

## Reproduction and data

```sh
PLTCOLLECTS="$PWD:" python miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --output ablation-local.json

PLTCOLLECTS="$PWD:" python miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --editions trs2 --workloads branching \
  --variants stream stream-no-cycle stream-no-result stream-no-reentrance stream-no-guards \
  --output guards-local.json
```

`ablation-results-20261107.json` contains 252 measured samples;
`guard-ablation-results-20261107.json` contains 49. All have the expected
answer counts, and each edition/workload's preview outputs agree.
