# TRS1 official stream readers (2026-11-08)

Baseline: `490daef4fdc295309a3b2b101a2ea91242eca735`, including the retained
query-reuse changes. Only TRS1 case∞ is changed in temporary candidate worktrees.
The production Kanren implementation and #16 stream extension are unchanged.

## Candidates and functional behavior

case∞ already calls stream-force and checks for empty. The search constructors
make eager, single-valued answer nodes, so official first/rest are semantically
usable at the remaining read sites. No replacement is made at readiness tests
or suspension boundaries in microKanren or TRS2.

- first.patch replaces both alternative (car (stream-peek-first a∞)) expressions
  with (stream-first a∞).
- rest.patch replaces the single reused (stream-peek-rest a∞) with
  (stream-rest a∞).
- both.patch makes both substitutions.

All three patches in official-readers-candidates/ apply to the baseline commit.
Each candidate passes 933 assertions from miniKanren/stream/trs1/tests and
microKanren/stream/tests/scheduling.rkt, covering book fixtures, cold/warmed
traces, cached choice tails, infinite branches, and untouched tails. Official
rest returns the same stored tail object in this valid search-stream domain;
it does not add a force of that tail.

The reader functions re-exported by private/racket/stream are the unchanged
Racket functions: eq? against racket/stream's stream-first and stream-rest
returns #t for both. This experiment does not substitute a custom fast reader.

## Same-round CPU results

Racket 9.3 CS, Linux x86_64. Three independent processes per backend/workload,
three measured samples per process, small/full warmups, and three full
collections before each sample. CPU includes query-time GC. Compilation,
type checking, loading, and preview validation are excluded. The original
checked/manual and original no-check/manual implementations are also recorded
as controls. Workload counts match PEEK-REUSE.md: one million answers, 5000
split queries, and 1000 seven-bit branching queries.

Table entries are medians of process medians, in ms:

| Workload | Current peek readers | Official first only | Official rest only | Both official |
| --- | ---: | ---: | ---: | ---: |
| answers | 343 | 575 | 488 | 708 |
| splits | 108 | 103 | 143 | 130 |
| branching | 164 | 214 | 224 | 266 |

All 162 samples have expected answer counts and matching preview outputs.
The single-digit split improvement for first-only is small and does not offset
its large regressions in the other two workloads. First-only and both-official
are slower than baseline in every independent process for answers and branching.
Rest-only is also slower in every process for those two workloads.

For million answers, per-process baseline/first/rest/both medians are:
345/533/480/654, 332/582/488/708, and 343/575/502/722 ms. Thus the conclusion is
not based on one selected fast/slow sample. These are same-round comparisons;
absolute baseline timings vary from earlier reports on the shared machine.

## Why the shorter expression is not necessarily the shorter execution path

The official reader's source in racket/private/for.rkt checks stream? and
stream-empty? before dispatching through the stream protocol. Official rest
also validates that the returned tail is a stream. case∞'s prior knowledge
that its node is mature/nonempty is not passed into these public functions.

For native eager nodes, the proposed peek readers directly check the local
node/wrapper state and access fields. The first peek exposes a list of cached
values, but avoiding that list at the call site did not produce a net gain here:
the full official-reader path is slower in the selected workloads.

In the million-answer case, baseline/first/rest/both GC medians are 88/179/89/183
ms, respectively. Rest-only's regression occurs with approximately unchanged
GC time; first/both also show increased GC time. This experiment does not isolate
allocations, peak retention, or compiler liveness effects, so it does not claim
that the source-level list packaging explains those GC differences.

The production implementation keeps its peek readers. None of these candidates
is adopted, and #16's source and APIs remain untouched. Earlier references to
using official readers were feasibility candidates, not prior timing results.

## Reproduce

Create worktrees at the baseline commit, apply the corresponding saved patches,
then run from the unmodified baseline with the updated driver:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --editions trs1 --processes 3 --rounds 3 \
  --variants stream stream-first stream-rest stream-both \
  --kanren-variant stream-first=/path/to/official-first-checkout \
  --kanren-variant stream-rest=/path/to/official-rest-checkout \
  --kanren-variant stream-both=/path/to/official-both-checkout \
  --output official-readers-local.json
```

Raw process samples, GC/real times, predicate controls, source/driver hashes,
and preview hashes are in official-readers-results-20261108.json.
