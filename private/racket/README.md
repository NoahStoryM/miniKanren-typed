# Native stream extension

```racket
(require private/racket/stream)
```

The facade exports the stock interface, replaces `stream-cons`, `stream-lazy`,
and `stream-force`, and adds three operations:

| Operation | Meaning |
| --- | --- |
| `stream-empty/known?` | True only if the stream is already known to be empty. |
| `stream-first-forced?` | True only if a first element is already available. |
| `stream-force-once` | Evaluate at most one pending local stream delay, cache its direct result, and return it. |

Both queries follow existing cached links without evaluating user expressions
or changing caches. False means "not known", including unsupported stream
implementations. Empty streams have no first element, so first-forced? is false.
A wrapper caching empty-stream is known empty even though it is not eq? to that
object. Heads that are themselves promises are returned as values, not forced.

Ordinary `stream-empty?` may force outer delays; the known-empty query does not.
Ordinary `stream-force` crosses a chain of local delays; force-once stops after
one new evaluation. Neither operation forces a cons node's lazy head. Successful
force-once also compresses its pre-existing cached prefix without a visited-node
list. There is no permanent pause history or side table. The enum-style
stream-inspect interface is no longer exported.

## Definitions to read

`private/stream-cons.rkt` imports the native cons macro and uses require/expose
for unchanged predicates/accessors, the head evaluator, and reentrant guard.
It redefines only the affected pieces:

1. The two-field delayed structure: its stream methods must close over the new
   stream-force; native method closures retain their original binding.
2. stream-cons and stream-lazy: construct local delays while delegating head
   evaluation and eager-tail cases to the native cons macro.
3. stream-force: follow cached immediate delayed results and compress links.
4. The three additions above, with private cache traversal/compression helpers.

The installed Racket module is not mutated. Stock derived operators/macros keep
their bindings and may produce stock delayed nodes, whose state this adapter
cannot inspect or step. The new protocol covers local delays and native cons
nodes (and recognizes lists); custom streams are conservatively unknown. Tested
on Racket 9.3 CS. This is not general gen:stream introspection.

`tests/stream.rkt` checks queries, exact exports, forcing, constructor choices,
sharing, and errors. `../../microKanren/stream/README.md` describes the search
abstraction and the backend that now uses this module. Historical benchmark
snapshots are under `../../experiments/stream-benchmarks/`; their old timings
are not fresh measurements of this adapter.
