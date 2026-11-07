# Three-backend comparison

Racket 9.3 CS, x86_64 shared host; seven samples per backend/case after small and
full warmups, fresh processes, full GC between samples. CPU time includes GC and
excludes compilation/module loading. Raw samples and exact source hashes are in
`results-20261107.json`. These are exploratory medians, not confidence bounds.

Baseline: `c2127b4fca7bca1fd85fcd53567823d959af25c3`.
Candidate implementation: `f2d6f048c1ae7228ce3cdb7cdc1294f2601791cd`.
The evidence commit changes only records/docs after that implementation.

| Workload | Size | Official ms | Baseline ms | Extension ms | Extension / baseline |
| --- | ---: | ---: | ---: | ---: | ---: |
| eager | 1,000,000 | 132 | 134 | 134 | 1.000 |
| lazy-tail | 1,000,000 | 270 | 259 | 266 | 1.027 |
| lazy-both | 1,000,000 | 288 | 267 | 286 | 1.071 |
| delay-chain | 2,000,000 | 193 | 217 | 211 | 0.972 |
| cached-head | 5,000,000 | 365 | 377 | 388 | 1.029 |

The original copy also differs from the installed library in some timings even
though its algorithms are identical. Module/code layout and shared-host noise
are confounders; do not attribute every percentage point to the extension.

Retained managed heap with the consumed source root kept alive:

| Workload | Official bytes | Baseline bytes | Extension bytes |
| --- | ---: | ---: | ---: |
| memory-chain, 300,000 | -1176 | -1192 | -1192 |
| memory-tail, 300,000 | 19186568 | 19186584 | 19186584 |

Small negative chain deltas are baseline/GC noise: all three are approximately
zero after full forcing compresses the chain. This is retained heap, not peak
memory or total allocation. The lazy-tail difference of a few bytes is noise.

These workloads exercise ordinary operators. The new peek-first singleton-list
allocation and single-step cycle/prefix traversal costs are not measured here.
Their behavior is covered by extension tests; further API-specific benchmarks
can be added after interface review.

Reproduce using the fixed baseline checkout and this candidate:

```sh
python private/racket/benchmarks/compare.py /path/to/racket \
  --baseline /path/to/baseline --candidate /path/to/candidate \
  --output /tmp/stream-comparison.json
```
