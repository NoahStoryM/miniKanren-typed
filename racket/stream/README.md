# Native Racket stream experiment

This experiment uses the installed `racket/stream`; it does not copy or patch
Racket's implementation. `private/representation.rkt` isolates `require/expose`
access to `racket/private/stream-cons`. These private names were tested with
Racket 9.3 CS and may change between releases. RackUnit is consequently a runtime
dependency of this experimental adapter.

The package has a multi-collection layout. The search-stream API is
`racket/stream/search`; the experimental logic programming entry points are
`microKanren/stream` and `miniKanren/stream/trs2`.
These experiments make no claim of Typed Racket support. The microKanren
implementation is untyped; the TRS2 implementation retains the reference's
type annotations under
`typed/racket/no-check` for comparison.

## Read the layers

- `search.rkt`: the API for exploring search streams.
- `private/stream-api.rkt`: representation-independent merge, bind, and observers.
- `private/representation.rkt`: native stream constructors and private queries.
- `../../microKanren/stream/private/core.rkt`: microKanren parametrized by the stream API.
- `../../microKanren/stream/main.rkt`: microKanren syntax and reification.
- `../../miniKanren/stream/trs2/main.rkt`: TRS2, including `ifte` and `once`.

A ready node uses `stream-cons` with **both** head and tail `#:eager`. Its answer
is stored directly, without a Box; its tail can itself be a suspended stream.
Empty is `'()`, which is also a Racket stream. `stream-unit` constructs one ready
answer. `stream-suspend` is a macro that defers its body.

`stream-inspect` never runs user code. It returns `end`, `ready(value, rest)`, or
`paused(resume)`. Merge keeps its left side after an answer and swaps sides at a
pause; bind uses this same merge. Ready prefixes are constructed eagerly, as in
the reference implementations. This is not element-by-element round robin.

```racket
(require (prefix-in search: racket/stream/search)
         racket/stream)
(define s
  (search:stream-merge
   (search:stream-suspend (search:stream-unit 'a))
   (search:stream-unit 'b)))
(search:stream-trace 10 s) ; '(pause (answer b) (answer a) end)
(stream->list s)          ; '(b a)
```

## Caching and compatibility

A logical pause remains a pause even after its body has been evaluated.
`stream-force-one` executes exactly one logical pause, memoizing its immediate
result with a standard `delay`. A native `stream-lazy` wraps that promise, and an
ephemeron-keyed side table connects the stream to the promise. This preserves
pause boundaries even if ordinary `stream-force` later compresses the native
delay chain. Retaining an experimental root can therefore retain intermediate
steps; dropping the root lets its records be collected. Ordinary native streams
are not entered into the table and their constructors/forcing are unchanged.
The extra promise, stream delay, and table entry do cost time and space for
experimental pauses; this is not a zero-overhead or benchmarked optimization.

`stream-head-forced?` reads a private field on a concrete ready node. It is a
physical query, separate from `stream-suspension?`, which identifies our logical
pause constructor. The adapter rejects foreign delayed streams rather than
forcing them during inspection or guessing their scheduling boundaries. Streams
built here can be consumed with ordinary Racket stream operations. Derived
streams from arbitrary native transformations need not be accepted back by this
search API.

This prototype memoizes pure search steps; the reference thunks do not.
Repeated evaluation of effectful goals or exceptions can therefore differ.
Ordinary consumption of an endlessly suspended stream still diverges; only the
search scheduler can yield to another branch at explicit pauses.

## Tests

The two `tests/main.rkt` entry points use `typed/racket/no-check` and `include`
to reuse the existing microKanren and all ten TRS2 chapters, including arithmetic.
The microKanren fixture's zero-argument `λ` constructor is rebound locally
at inclusion time to build native suspensions. TRS2 chapter 10 additionally uses
a test-only view for its explicit `pair?` / `car` / `cdr` / `procedure?` / `assert`
observations and converts finite streams to lists for equality checks. Its
explicit zero-argument `lambda` also constructs a native suspension. Chapters
1–9 need no such observer adaptation. The shared fixtures and expected answers
are not copied or edited. `tests/stream.rkt` independently checks native shapes
and one-step forcing directly, without the chapter-10 view.

`tests/scheduling.rkt` additionally compares generated merge/bind expressions
and recursive programs with the original implementation, checking answer order
and pause positions before and after ordinary native stream consumption.

From the repository root, with the usual Racket packages installed:

```sh
raco test microKanren/stream/tests/main.rkt \
          racket/stream/tests/scheduling.rkt \
          racket/stream/tests/stream.rkt \
          miniKanren/stream/trs2/tests/main.rkt
```

## Native-library extension experiment

`racket/stream/two-state` separately experiments with two additions to the native
library itself: non-forcing `stream-inspect` and `stream-force-one`. It retains
the two-state primitive representation and uses a small licensed Racket source
fork. It does not use the search adapter's history table or replace its API.
See [benchmarks/README.md](benchmarks/README.md) for the changes, measurements,
limitations, and reproduction commands.
