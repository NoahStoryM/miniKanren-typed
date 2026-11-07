# Direct Kanren comparison (2026-11-07)

Measured implementation commit: `607368a22517b5ae20307ed02dc0b968978c417a`.
Racket 9.3 CS, Linux x86_64. Raw samples, CPU/real/GC times, answer counts,
preview hashes, and implementation-source SHA256 hashes are in
`results-20261107.json`.

## Source and scheduling correspondence

The stream variants retain the original type annotations and non-stream
function definitions under `typed/racket/no-check`. TRS1 retains `case∞`'s
empty/single-answer/choice distinction, `mplus`/`bind`, and `mplusi`/`bindi`.
Its choice tail object stays distinct from unit's canonical empty tail, even
after forcing. TRS2 retains `append-inf`, `append-map-inf`, `take-inf`, `ifte`,
and `once`, using the same original suspension sites. Cache-link branches
follow already-computed links without inserting an extra pause.

Validation before these measurements: 605 original fixture assertions and
641 direct regression assertions pass. The latter compare 250 generated
expressions per microKanren/TRS2 for both answers and pauses, test TRS1's
four merge/bind operators, and cover consecutive suspensions, cooperative
infinite branches, ifte/once, unneeded tails, and warmed-cache behavior.
These are tests of the intended scheduling correspondence, not a proof for
all programs. Memoization still changes effects and the behavior of reusing
warmed inputs, as described in the parent README.

## Method

Three backends per edition:

- Original `typed/racket/base`, unchanged.
- The same original source with only its language changed to
  `typed/racket/no-check` in a temporary copy.
- The new stream implementation, also using `typed/racket/no-check`.

The same benchmark relations and timer code are appended inside each
implementation module. No typed/untyped boundary is crossed inside the timed
region. Compilation, type checking, module loading, and answer-preview checking
are excluded. Each backend/workload runs in a fresh process, after a small
warmup and a full-size warmup. Seven measured samples each follow three full
collections; CPU time includes GC during the query. No stream is reused between
samples. Backend order rotates across workloads.

Workloads include goal construction, search, reification, result-list creation,
and counting the answers:

- `answers`: take 1,000,000 answers from a recursive relation returning `a`.
- `splits`: enumerate the 13 splits of a 12-element list, 5,000 fresh queries
  (65,000 total answers).
- `branching`: enumerate all 128 seven-bit lists, 250 fresh queries
  (32,000 total answers).

All 189 timed samples have the expected answer counts. Untimed previews also
match exactly across the three backends of each edition/workload.

## Results

Median process CPU time, milliseconds. Ratios compare the two no-check
implementations to keep the language mode constant.

| Edition | Workload | Original TR | Original no-check | Stream no-check | Stream / original no-check |
| --- | --- | ---: | ---: | ---: | ---: |
| micro | answers | 360 | 399 | 1270 | 3.18× |
| micro | splits | 156 | 163 | 306 | 1.88× |
| micro | branching | 49 | 54 | 336 | 6.22× |
| trs1 | answers | 259 | 111 | 637 | 5.74× |
| trs1 | splits | 821 | 80 | 125 | 1.56× |
| trs1 | branching | 87 | 17 | 55 | 3.24× |
| trs2 | answers | 344 | 267 | 592 | 2.22× |
| trs2 | splits | 724 | 82 | 124 | 1.51× |
| trs2 | branching | 101 | 23 | 117 | 5.09× |

The observed stream/no-check ratios range from 1.51× to 6.22×. These workloads
are a small exploratory sample on a shared machine, not an overall slowdown
factor for Kanren. Some original typed/no-check timings differ substantially;
this is a compiled-runtime difference, not type-checking time in the samples.
The cause of that difference has not been profiled.

This comparison differs from the earlier stream-library benchmark: that one
compared the same native stream representation before and after the API
extension. This one replaces compact pair/thunk or Box/pair/thunk representations
with native stream structures and their operations throughout full queries.

From the implementation, candidate overhead includes cache/state and safety
checks, delay objects around closures, and allocating the values list returned
by `stream-peek-first`. Some operations currently call that query twice per
answer. These are plausible optimization targets, not a profile-derived
allocation of the measured costs. This measurement does not apply any new
optimizations to the implementations.

## Reproduction

From the repository root, with the Racket dependencies available:

```sh
PLTCOLLECTS="$PWD:" python miniKanren/stream/benchmarks/compare.py \
  "$(command -v racket)" --rounds 7 \
  --output miniKanren/stream/benchmarks/results-local.json
```

The driver constructs temporary copies and leaves the original implementations
unchanged. It compiles all variants before measuring them.
