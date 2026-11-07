# Cache-query dispatch experiment (2026-11-07)

The baseline includes the merged identity-cycle fast path from #18:
stream-extension commit `1e1896b3adf54182305129353add21f87d055b63`, integrated
into Kanren research commit `009a67781825a27d6701f88436f16f928a5e82c7`.
No predicate, relation, scheduler, or stream storage layout changes are mixed
into these candidates.

## Candidates

The four original patches in `dispatch-candidates/` apply to the extension baseline above.
The candidate-library hashes in the raw reports identify their exact contents.

- `query.patch`: self-recursive first/rest peeks combine cached-link traversal
  with terminal-node dispatch, avoiding the helper followed by another dispatch.
- `force.patch`: combine the endpoint walk and pending-state dispatch in
  stream-force-once; retain all result, reentrancy, and cache-cycle guards.
- `combined.patch`: both changes above, with indentation and line wrapping fixed.
- `loop.patch`: combine first/rest traversal and dispatch in a local tail loop.
  The loop does not capture the original argument solely to report an error:
  every cached edge already points to a valid stream, so invalid input can
  only be the initial argument. Impersonators remain opaque before field reads.

None of the candidates forces a stream during a peek or compresses its cache
links. First still returns #f for an unreadable head, a list of all cached
values otherwise; rest returns the stored tail stream object. Zero/multiple
values, #f answers, proper lists, generic streams, and opaque wrappers retain
their existing behavior. The helper remains for stream-force-once in the
query-only candidates.

## Measurement

Racket 9.3 CS, Linux x86_64. Every backend uses the same handwritten Data
predicate; the checked original and pair/thunk no-check implementations are
controls. Compiling, loading, type checking, and preview validation occur
outside timing. CPU time includes query-time GC. All samples construct fresh
queries; no previously warmed result stream is reused.

`dispatch-results-20261107.json` compares query, force, and baseline in that
order (rotating across workloads), with seven samples per fresh process.
`dispatch-repeat-results-20261107.json` starts with baseline, then query and
combined, again with seven samples. Several apparent improvements disappear
or reverse between these runs: e.g. TRS2 branching query/baseline is 290/265 ms
in the first run and 271/286 ms in the second. One process with seven samples
is insufficient evidence for a small general speedup.

`dispatch-process-results-20261107.json` adds three independent processes per
backend/workload, five measured samples per process, and rotates backend order
across both workloads and process repetitions. Each process receives small
and full-size warmups, and each sample follows three full collections. The
summary below uses the median of the three process CPU medians; the raw file
also records GC times, individual samples, answer counts, and preview hashes.

CPU medians in milliseconds (negative change is faster):

| Edition | Workload | #18 baseline | Query local loop | Combined | Loop change |
| --- | --- | ---: | ---: | ---: | ---: |
| trs1 | answers | 498 | 449 | 442 | -9.8% |
| trs1 | splits | 101 | 104 | 103 | +3.0% |
| trs1 | branching | 184 | 167 | 167 | -9.2% |
| trs2 | answers | 590 | 503 | 573 | -14.7% |
| trs2 | splits | 121 | 108 | 118 | -10.7% |
| trs2 | branching | 270 | 268 | 243 | -0.7% |

The local-loop query candidate is retained for review. It reduces the repeated
endpoint classification on ready nodes without touching stream-force-once,
cache writes, guards, constructors, or the native stream operators. It improves
the million-answer cases in all three process comparisons, but not every
workload: TRS1 splits is approximately unchanged/slightly slower and TRS2
branching has no consistent gain (268/270, 249/289, and 273/262 ms by process).
Do not describe it as a universal speedup or use the best individual run.

The combined candidate improves branching in this run, but its extra force
refactor does not consistently improve on the smaller query-only change:
TRS2 answers medians are 655, 573, 557 ms, versus 535, 486, 503 ms for the query
loop, and 590, 562, 591 ms for baseline. That force change is left experimental.
The direct self-recursive query candidate is also left experimental rather
than stacking several equivalent dispatch rewrites in production.

All preview answers and sample answer counts agree (450 measured samples).
These workloads are the same 1,000,000-answer, 5,000 split-query, and 1,000
seven-bit branching-query cases documented in ABLATION.md. Compilation is
excluded from every measurement. The report preserves the original TR and
no-check controls, rather than attributing differences to type checking.

The retained local-loop candidate passes 833 stream assertions plus 1284
Kanren assertions (2117 total) using an isolated collection override, including
both editions' unchanged book fixtures and suspension traces. The measured
primitive hash matches the recorded loop patch. The implementation is isolated
in [PR #19](https://github.com/NoahStoryM/miniKanren-typed/pull/19), commit
`3a782e9a530848100ad384e56e7a67d85cd67a46`, targeting the extension branch.
The Kanren research branch keeps the #18 library baseline so these comparisons
remain reproducible; #19 is not merged into that baseline here.


## Reproduce

Use a checkout of the Kanren research branch as the working directory. Create
separate worktrees of extension commit 1e1896b and apply the relevant saved
patches there, then run:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --processes 3 --rounds 5 \
  --variants stream-combined stream-loop stream \
  --library-variant stream-combined=/path/to/combined-checkout \
  --library-variant stream-loop=/path/to/loop-checkout \
  --output dispatch-local.json
```

The baseline Kanren checkout must still contain the #18 stream implementation.
The driver copies each candidate library to an isolated temporary directory;
no package replacement or global installation changes are required. The
stream-fast-cycle diagnostic is now a no-op on a baseline that already has
#18, rather than nesting its fast path again.

## Localize the sole remaining endpoint walk (2026-11-08)

After the query-loop change, only stream-force-once used cached-endpoint.
Commit `67181269c1134c0cbb2e18247949b9737e913591` removes that module-level
helper and places the same tail loop in stream-force-once's local definition
of s. The traversal and forcing dispatch remain separate stages. No additional
state checks or forcing-branch fusion from force.patch are adopted.

`dispatch-candidates/local-scope.patch` applies to the preceding query-loop
commit `3a782e9a530848100ad384e56e7a67d85cd67a46`, rather than the #18 baseline.
All 833 stream and 1284 Kanren assertions pass again with this final candidate.

`local-scope-results-20261108.json` spot-checks the force-heavy TRS2 branching
workload with three independent processes and five samples each. This is a
separate before/after scope check, not a repeat of the entire six-case matrix.
CPU medians in ms:

| Process | Before localization | Local walk |
| --- | ---: | ---: |
| 1 | 252 | 261 |
| 2 | 241 | 241 |
| 3 | 247 | 239 |

The process medians vary in both directions; this check establishes neither
a consistent regression nor a new speedup. Compilation and loading remain
excluded, and CPU includes GC. All 60 measured samples (including the checked
original and no-check controls) have matching counts and preview answers.

To repeat, create worktrees of the before/after commits, then use:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --editions trs2 --workloads branching \
  --processes 3 --rounds 5 --variants stream-before stream-local \
  --library-variant stream-before=/path/to/query-loop-checkout \
  --library-variant stream-local=/path/to/local-scope-checkout \
  --output local-scope-local.json
```

## Fuse cached traversal and single-step dispatch (2026-11-08)

The current PR #19 head, `d05d40715dacf025f66ed59bdef1a6143ee95293`, combines
the local endpoint walk and forcing cond into one tail loop. Its first branch
recognizes a plain local delayed node and reads creator-if-unforced once:
#f follows the cached link; a non-#f marker enters the existing forcing body.
The loop retains the original source for prefix compression and error reporting,
and uses the current s for cycle detection. All forcing guards remain intact.
This is the requested single-argument loop, not the earlier two-argument
root-carrying experimental force.patch.

`dispatch-candidates/force-loop.patch` applies to the preceding scope-only
commit `67181269c1134c0cbb2e18247949b9737e913591`. All 833 stream assertions
and 1284 Kanren assertions pass again with the fused version (2117 total).

`force-loop-results-20261108.json` compares that preceding version with the
fused version on both editions' branching workload: 1000 fresh queries,
128000 answers per sample, three independent processes, five samples each.
Compiling, loading, and type checking are excluded; CPU includes GC. Per-process
CPU medians in ms:

| Edition | Process | Before fusion | Fused |
| --- | ---: | ---: | ---: |
| TRS1 | 1 | 182 | 160 |
| TRS1 | 2 | 177 | 184 |
| TRS1 | 3 | 192 | 185 |
| TRS2 | 1 | 251 | 243 |
| TRS2 | 2 | 247 | 240 |
| TRS2 | 3 | 251 | 243 |

The median of process medians is 182 → 184 ms for TRS1 and 251 → 243 ms
for TRS2. TRS1 is approximately unchanged, with variation in both directions;
TRS2 improves by about 3% in this run. This is primarily a structural cleanup,
not evidence of a universal speedup. It does not remeasure the earlier full
six-case matrix against the final head. All 120 samples, including checked TR
and no-check controls, have matching counts and preview answers.

Reproduce with before/after worktrees of the two commits above:

```sh
PLTCOLLECTS="$PWD:" python3 miniKanren/stream/benchmarks/ablation.py \
  "$(command -v racket)" --workloads branching --processes 3 --rounds 5 \
  --variants stream-before stream-fused \
  --library-variant stream-before=/path/to/before-fusion-checkout \
  --library-variant stream-fused=/path/to/fused-checkout \
  --output force-loop-local.json
```
