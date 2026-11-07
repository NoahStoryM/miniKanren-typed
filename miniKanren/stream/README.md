# Direct Kanren ports to `private/racket/stream`

```racket
(require microKanren/stream)      ; μKanren
(require miniKanren/stream/trs1)  ; first edition
(require miniKanren/stream/trs2)  ; second edition
```

Use one implementation per module, or prefix the imports. The implementations
and entry modules use `typed/racket/no-check`, retaining the corresponding
`typed/` source's type annotations, typed parameters, `ann`,
and macro forms for comparison. No type checking is performed, and no
`typed-racket-stream` dependency is introduced.

There is no new stream algebra, backend interface, or shared Kanren core.
Each `main.rkt` keeps the original definitions in their original order.
Unification, substitutions, variable identity, goal composition, and reification
retain their original definitions. The ground-data predicate is the exception:
Racket 9.3's `define-predicate` does not generate the requested predicate under
no-check. `data?` therefore spells out the same ground-term test explicitly,
with its original predicate type annotation. Regression tests compare its
results with the original checked predicate independently of query answers.
TRS2 `defrel` again supplies the original parameter and result annotations.

`Streamof` uses `(Sequenceof a)` as a conservative description of the native,
single-valued streams constructed here. It also admits other sequences: this
alias does not characterize the representation precisely or make those other
sequences valid search-stream inputs. `Suspension` aliases `Streamof`, since a
suspension is now a delayed stream object rather than a function. The types do
not distinguish pending, cached, empty, or ready streams. μKanren's `mzero` is
annotated `(Streamof Nothing)`, and `pull` returns `Streamof` rather than a
pair/null union. These are design annotations, not checked stream bindings.

## Source comparisons

| Original | Direct stream variant |
| --- | --- |
| `typed/microKanren/main.rkt` | `microKanren/stream/main.rkt` |
| `typed/miniKanren/trs1/main.rkt` | `miniKanren/stream/trs1/main.rkt` |
| `typed/miniKanren/trs2/main.rkt` | `miniKanren/stream/trs2/main.rkt` |

| Original stream operation | Replacement |
| --- | --- |
| Empty stream | `empty-stream` |
| Ready pair | `stream-cons #:eager answer #:eager rest` |
| Suspension / zero-argument lambda | `stream-lazy` |
| One suspension call | `stream-force-once` |
| `pull`, repeated suspension calls | `stream-force` |
| Ready answer | `(car (stream-peek-first s))` |
| Tail | `(stream-peek-rest s)` |

`stream-peek-first` returns cached values as a list. The remaining `car` unwraps
that list; it does not inspect the physical stream representation. Pairs in
terms, substitutions, states, and extracted result lists remain ordinary pairs.

Both fields of ready nodes use `#:eager`: the original `cons` evaluates both
arguments. A lazy default head or tail would introduce new scheduling points.
Only original suspension sites become `stream-lazy`.

## Empty streams and existing caches

The implementations construct eager, single-valued answer nodes and explicit
suspensions, terminated by the canonical `empty-stream`. They do not accept
arbitrary custom streams or delayed answer expressions as search streams.
Racket's public `empty-stream` is a generic stream object, not `'()`.
The non-forcing empty test is `(eq? (stream-peek s) empty-stream)`.

A cached delay may point to another delay or to `empty-stream`. The extra
`[(stream-peek s) => ...]` branches follow those existing links without creating
a new suspension or evaluating a thunk. `stream-empty?` is used only where the
original algorithm already demands a mature stream, such as after `pull`.

## TRS1's single-answer case

TRS1 distinguishes `unit` from `choice`, unlike μKanren/TRS2. Here `unit` has
`empty-stream` as its immediate tail; `choice` has a suspended tail object.
`case∞` tests that tail's identity without forcing it. Even if a choice tail
is later forced to empty, the node remains a choice. No Box or additional tag
is needed. The original `λF` and `λG` macro names remain for comparison;
`λF` now constructs a stream suspension.

## Memoization

Fresh runs preserve the tested answer order and suspension traces. Racket
streams memoize, whereas the original thunks do not. Reusing a warmed input
observes its current cache: an old pause may already contain an answer, which
can change interleaving in a newly constructed merge. Re-reading a result uses
its cached answers. Effects and failed computations also follow Racket's stream
memoization semantics. These differences are intentional and tested.

## Tests

```sh
raco test microKanren/stream/tests/main.rkt \
          miniKanren/stream/trs1/tests/main.rkt \
          miniKanren/stream/trs2/tests/main.rkt \
          microKanren/stream/tests/scheduling.rkt
```

The test entry points use `typed/racket/no-check` and include the existing
fixtures directly. Expected answers and relation definitions are unchanged.
μKanren's explicit zero-argument lambdas become `stream-lazy` at the inclusion
boundary. TRS2 chapter 10 uses local adapters for pair/thunk observations and
explicit suspension construction; earlier chapters need no representation
adapters. The separate scheduling tests operate directly on native streams,
compare cold answer/pause traces with the original implementations, and cover
infinite branches, cached inputs, and untouched tails.

## Runtime comparison

See [the latest caller-side query-reuse results](benchmarks/PEEK-REUSE.md)
for the current TRS1/TRS2 comparison. The stream-library integration baseline
is preserved in [FINAL.md](benchmarks/FINAL.md). Checked-original and
no-check-original controls use the same handwritten Data predicate as the
stream implementation. Compilation and loading are excluded; independent
process results, source hashes, reproducible scripts, and raw samples are
included. Historical predicate/dispatch experiments retain their recorded
source revisions.
