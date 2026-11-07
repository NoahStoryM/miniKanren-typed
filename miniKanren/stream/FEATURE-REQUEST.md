# [Feature Request] Non-forcing inspection and single-step forcing of native streams

Discussion draft for independent review; not submitted upstream.

**Is your feature request related to a problem? Please describe.**

I am experimenting with implementing microKanren and the two editions of *The Reasoned Schemer* using Racket’s native stream constructors, while preserving their scheduling algorithms and making minimal changes to the Kanren code.

These algorithms distinguish ready answers from suspended computations. Encountering a suspension can cause the scheduler to switch to another branch instead of continuing until the current branch produces an answer.

For example:

```racket
(define (waiting)
  (stream-lazy (waiting)))

(define ready
  (stream-cons #:eager 'answer #:eager empty-stream))
```

Each delayed expression in `waiting` terminates by producing another delayed stream. A cooperative merge should be able to advance this branch once, then obtain `'answer` from `ready`. However, calling `stream-empty?` on the left branch keeps unfolding it without returning control to the scheduler.

The missing capabilities appear to be non-forcing observation of native stream state and advancement through one explicit suspension boundary.

This is specifically about native streams. The general `gen:stream` interface provides empty/first/rest operations, but does not require an implementation to expose caching or suspension state.

**Describe the solution you'd like**

I would like public operations for inspecting and advancing the native delay mechanism used by `stream-cons` and `stream-lazy`.

The existing `stream-force` provides a useful precedent: it unfolds native delayed-stream wrappers, but returns a custom stream object without attempting to force computations hidden inside its methods. I am exploring an extension with a similarly limited scope.

The experimental API consists of:

- `stream-peek`: read one available native delay-cache link without evaluation. Objects that are not native delay wrappers are returned unchanged, except where wrapping prevents safe observation.
- `stream-peek-first`: return available first-element values as a list, or `#f` when they cannot be read without evaluation. A cached `#f` is represented by `'(#f)`.
- `stream-peek-rest`: return a known cons node’s stored tail stream without forcing it. The returned tail may itself be delayed.
- `stream-force-once`: follow existing cached links, invoke at most one pending native stream-producing thunk, and return its immediate result without recursively forcing that result.

These operations do not make arbitrary streams cooperatively schedulable. Custom methods may perform unbounded work, and a single delayed expression may itself diverge. Applications must supply appropriate suspension points.

The prototype supports native nodes and lists. First/rest peeks report unavailable for opaque custom or wrapped streams, and single-step forcing does not invoke custom stream methods. “Unavailable” therefore does not imply “not yet evaluated.”

These are candidate interfaces. I am open to different names, return conventions, or module placement. In particular, I would appreciate feedback on whether observing native suspension boundaries fits the intended abstraction. I am not proposing that all `gen:stream` implementations acquire new required methods.

**Describe alternatives you've considered**

- **Ordinary stream reads or `stream-force`:** these can unfold arbitrarily many suspensions before returning.
- **The original pair/thunk representation:** this directly supports the algorithms, but maintains a separate stream representation.
- **Custom streams, promises, or additional bookkeeping:** viable, but require separate suspension/state machinery instead of directly reusing the native constructors.
- **Accessing private stream implementation details:** sufficient for experimentation, but not a supported interface.

The examples provide strong motivation for state observation and single-step advancement. They do not establish that all four proposed functions are individually indispensable; the first/rest peeks also form a convenient non-forcing observation interface.

**Do you want to contribute to this feature**

Yes. I have a working experimental implementation and can contribute implementation work, tests, documentation, and benchmarks. I would like to discuss the scope and semantics before preparing an upstream PR.

**[Optional] Additional context**

The experiment is split into two PRs:

- [Stream extension — Files changed](https://github.com/NoahStoryM/miniKanren-typed/pull/16/files): isolates the extension from a copied Racket 9.3 baseline. It preserves the existing structure layouts and constructors, adds the four operations, and adjusts `stream-force` to handle cached links to other native delayed streams.
- [Kanren applications and experiments](https://github.com/NoahStoryM/miniKanren-typed/pull/17): contains the microKanren/TRS1/TRS2 ports, book fixtures, scheduling tests, and reproducible performance experiments.

The integrated experiment has 833 stream assertions and 1284 Kanren assertions. Fresh searches reproduce the tested answer orders and suspension traces. Native memoization is retained, so warmed/shared streams follow their current cache state rather than the original non-memoizing thunk behavior.

The implementation and measurements are feasibility evidence, not a claim that the API is final or has zero overhead.
