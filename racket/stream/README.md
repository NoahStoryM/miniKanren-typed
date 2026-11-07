# Minimal native-stream extension

```racket
(require racket/stream/main)
```

`main.rkt` exports everything provided by the installed `racket/stream`, replacing
`stream-cons`, `stream-lazy`, and `stream-force`, and adding only
`stream-inspect` and `stream-force-once`. It does not patch the installed module.

Read `private/stream-cons.rkt` for the changes:

1. Require/expose the unchanged native cons predicate/accessors, head evaluator,
   and reentrant guard; import the native stream-cons macro normally.
2. Redefine the delayed-node structure with the same two fields. Its stream
   methods must call the new force implementation; native method closures retain
   the original binding, so merely defining another stream-force is insufficient.
3. Redefine stream-cons and stream-lazy to construct those delayed nodes.
   stream-cons delegates head evaluation and eager-tail cases to the native macro.
4. Redefine stream-force to follow immediate cached delayed results, retaining
   ordinary full forcing and path compression.
5. Add stream-inspect and stream-force-once, including cached-prefix compression.

The observer executes no user code: empty / ready / head-lazy / lazy are normal
states; blocked reflects the native running/broken guard; unknown covers stream
implementations whose state this module cannot inspect. The single-step operator
executes at most one pending local delayed expression, caches its immediate
result, and compresses previously cached links. It never forces a lazy head.

Stock derived operators and macros retain their original bindings and can
produce stock delayed nodes, which report unknown here. The new observation and
stepping protocol applies to delays made by this module's constructors; the
ordinary stream protocol remains usable in both directions. This private adapter
was tested with Racket 9.3 CS; it is not a general gen:stream introspection API.

Tests are in `tests/main.rkt`. Historical benchmarks and the full primitive
snapshot are in `../../experiments/stream-benchmarks/`. The earlier search API
and its separate history-tracking adapter are in
`../../experiments/search-stream/`; they are not part of this module's exports.
