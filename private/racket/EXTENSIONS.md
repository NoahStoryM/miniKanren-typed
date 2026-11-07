# Cache observation and one-step forcing

This branch targets `research/stream`, not master. Its base is the unextended
Racket v9.3 copy at c2127b4fca7bca1fd85fcd53567823d959af25c3.
The three-import relocation and all source copying belong to that baseline;
this diff contains only the extension, its tests/docs, and benchmark evidence.

```racket
(require private/racket/stream)
```

| Operation | Result without additional evaluation |
| --- | --- |
| stream-peek | One cached stream link; #f for a pending native delay. A non-delayed stream is returned unchanged. |
| stream-peek-first | List of all cached first-element values, or #f when no readable head is available. |
| stream-peek-rest | Tail stream object of a known cons node, or #f when unavailable. The returned tail can still be delayed. |
| stream-force-once | Follow existing caches, evaluate at most one pending native stream expression, cache its immediate result and compress the old prefix. |

All three peeks are read-only and run no user expressions/methods. First/rest
peeks follow cached outer links but do not compress them. A single #f head is
represented by '(#f), a single list by a singleton list containing it, multiple
values by a list of those values, and zero values by '(). Empty streams have no
head or tail and give #f for first/rest peeks. Lists are supported as streams.

Rest means cdr, not last: retrieving its wrapper does not imply evaluating the
rest expression. Apply stream-peek to that wrapper to read its cache.

## Examples and derived predicates

```racket
(define s (stream-cons #f empty-stream))
(stream-peek-first s) ; #f: head is pending
(stream-first s)      ; #f: this is the actual answer
(stream-peek-first s) ; '(#f): answer is now cached

(define p (stream-lazy empty-stream))
(stream-peek p)       ; #f
(stream-force-once p) ; empty-stream
(stream-peek p)       ; empty-stream; p itself is still a wrapper

(define (first-forced? s)
  (if (stream-peek-first s) #t #f))

(define (empty/known? s)
  (cond
    [(or (eq? s empty-stream) (null? s)) #t]
    [else
     (define cached (stream-peek s))
     (and cached (not (eq? cached s)) (empty/known? cached))]))
```

No separate stream-empty/known? or stream-first-forced? is exported. A generic
custom stream is opaque to first/rest peeks; stream-peek returns that object and
force-once leaves it unchanged. Impersonators/chaperones are opaque to all peeks
(#f) and to single-step forcing (returned unchanged), so cache access cannot
bypass contract checks. Ordinary stream operations remain responsible for their
normal protocol. Running/failed native delays expose no readable cache and
retain the existing reentrant/broken guard.

## Implementation scope

Only the public import/export list and the primitive implementation change.
The two original structure layouts, constructor macros, head forcing, shared
protocol, and all derived operators are unchanged. The existing stream-force
cache-hit branch now follows a cached native delay instead of returning it as a
final result. Four functions and private traversal/compression helpers are added.
There are no new fields, tags, promises, side tables, or permanent pause history.

First peeks may allocate a singleton list for a single cached value. This cost
belongs to the new query, not ordinary head reads. The ordinary forcing path
adds a native-delay type check on cached results. One-step cycle checking can
traverse cached targets; the old input prefix is compressed without a temporary
visited-node list. This is a research API, not a claim of final upstream design.

## Validation and next stage

Run compatibility and extension tests with Racket 9.3:

```sh
raco test private/racket/tests/compatibility.rkt private/racket/tests/extensions.rkt
```

There are 829 passing assertions across the shared stock/copy compatibility
fixtures and extension cases: values, false/procedure heads, tail laziness,
read-only cached links, copied derived operators, contracts, opaque streams,
cycle/failure protection, old-root compression, and seeded finite expressions.

Compare installed Racket, the fixed baseline checkout, and this candidate with
benchmarks/compare.py. Keep the baseline immutable when collecting results.
Kanren changes are deliberately a later branch/PR; no Kanren implementation is
included in this extension diff. Earlier #14 is retained as exploration history.
