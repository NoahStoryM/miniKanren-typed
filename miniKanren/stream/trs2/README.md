# TRS2 on the extended native stream backend

```racket
(require miniKanren/stream/trs2)
(run* q (conde [(== q 'olive)] [(== q 'oil)])) ; '(olive oil)
```

This unchecked prototype implements the second edition's goal operations using
the same search-stream abstraction/native adapter as `microKanren/stream`.
The adapter imports `private/racket/stream`: non-forcing known-empty/first-forced
queries, native cons/lazy constructors, and single-step forcing. There is no
history table, extra promise layer, Box, or dependency on the old adapter in
`experiments/search-stream/`.

`append-inf` and `append-map-inf` use the shared abstract merge/bind operations.
`ifte` and `once` inspect abstract end/ready/paused cases and preserve suspension
when waiting for a decision. `defrel` suspends constructing and applying its goal
body. Failure and empty results use the native empty-stream; substitutions still
use ordinary lists. Term operations and the occurs check are unchanged.

Like the microKanren backend, this observes current cache state, not historical
pause identity. Cold executions are compared with the book's expected answers;
warmed input streams are not promised to reproduce old thunk scheduling events.
Type annotations remain as documentation under typed/racket/no-check; this does
not claim static Typed Racket support.

## Book tests

`tests/main.rkt` uses typed/racket/no-check and include for the existing fixtures
in `typed/miniKanren/trs2/tests/`: chapters 1–10 and the arithmetic relations.
Those fixtures and expected answers are unchanged. Chapters 1–9 need no observer
adaptation. Chapter 6 includes the book's million-answer very-recursiveo test.

Chapter 10 intentionally examines the book's pair/thunk representation. Its
inclusion scope supplies a test-only observer view: ready native nodes act as
pairs, pending native nodes as callable suspensions, and finite native results
are converted to lists for equality checks. Explicit zero-argument lambda
suspensions use native stream-lazy. The view preserves ordinary term/substitution
pairs and does not modify any relation algorithm or expected answer.

`tests/native.rkt` separately checks the real native representation without that
view: relation delays, cache queries, canonical failure, consecutive pauses in
ifte/once, skipping an unneeded tail, and a cooperatively infinite left branch.

```sh
raco test miniKanren/stream/trs2/tests/main.rkt \
          miniKanren/stream/trs2/tests/native.rkt
```
