# microKanren through a search-stream abstraction

```racket
(require microKanren/stream)
(run* (q) (conde [(== q 'a)] [(== q 'b)])) ; '(a b)
```

There are three separate layers:

1. `private/racket/stream`: native stream constructors/consumers, plus non-forcing
   known-empty and first-forced queries, and stream-force-once.
2. `private/stream-api.rkt`: representation-independent search algebra.
   `private/native-stream.rkt` implements its four primitives using layer 1.
3. `private/core.rkt`: unification goals, fresh variables, disjunction, and
   conjunction, parametrized by the stream API. `main.rkt` supplies the native
   backend and user syntax. Term/state operations are in `private/terms.rkt`.

## Four representation primitives

| Primitive | Contract |
| --- | --- |
| empty | Empty search stream. |
| emit(answer, rest) | Construct a ready answer with an already constructed tail object. |
| suspend(thunk) | Defer producing a search stream. |
| inspect(source) | Without executing user code, return end, ready(answer, rest), or paused(resume). |

The native implementation is small: empty is empty-stream; emit uses
stream-cons with both fields eager; suspend uses the extended stream-lazy.
Inspect first checks stream-empty/known?, then stream-first-forced?. A ready
node can be read using ordinary stream-first/rest; the remaining case provides
a resume thunk calling stream-force-once. No private fields, raw-thunk stream
representation, promise layer, Box, or side table is used by this backend.
Only streams constructed by these primitives are valid search streams: in
particular, a ready node's answer must already be available. Foreign streams and
lazy-head cons nodes are not silently forced to guess their search behavior.

The generic algebra derives unit, merge (mplus), bind, pull, answer extraction,
and a diagnostic trace from those primitives. Merge keeps the left side after
an answer and swaps sides at a pause. Bind uses this same merge. Construction
of ready prefixes remains eager, matching the reference implementation.

The core uses only empty/unit/suspend/merge/bind and never asks whether a search
stream is a pair or procedure. Pairs in terms, substitutions, and states are
independent of the stream representation. Tests instantiate the same core on a
pair/thunk backend as well as the native backend.

## Cache semantics

Inspection describes current state, not historical constructor identity.
A resumed delay can subsequently inspect as ready or empty. Reusing a warmed
input in a new merge can consequently change its interleaving: merging a paused
left answer with a ready right answer yields right then left, while warming that
left input first yields left then right. This implementation does not replay
past pause boundaries, unlike the earlier experiment under
`../../experiments/search-stream/`.

Fresh cold executions match the reference's tested answer order and pause
traces. Ordinary native stream consumption can warm/flatten delays; reading the
already-built result again returns its cached answers, without old pause events.
Memoization also changes repeated effects and failure behavior compared with
uncached reference thunks. These are deliberate limits of the cache-state model.

## Validation

`tests/main.rkt` uses typed/racket/no-check and include to reuse all original
microKanren fixtures and assertions. Their explicit zero-argument lambdas are
locally rebound to native suspension construction; the test-only cdr observer
maps a known empty tail to the reference's '(). Expected answers are unchanged.

`tests/scheduling.rkt` compares seeded merge/bind expressions and eight recursive
programs with the original implementation, including infinite cooperative
branches. It checks the alternative pair/thunk backend, cached empty results,
warming semantics, and interoperability with ordinary stream consumers.

```sh
raco test private/racket/tests/stream.rkt \
          microKanren/stream/tests/main.rkt \
          microKanren/stream/tests/scheduling.rkt
```

`miniKanren/stream/trs2` now uses the same native adapter and abstract algebra.
Its book tests include all ten chapter fixtures and arithmetic; its own README
explains the representation-only observer view used for chapter 10.
