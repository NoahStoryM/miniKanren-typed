# Cost of the public stream operations (experiment record)

Not filed upstream; kept here in case it becomes useful.

## Question

The `stream-pause` ports use only `stream-empty?`, `stream-first` and
`stream-rest`, and are 1.7–5× slower than the `stream` ports, which read
nodes through the peek extension. Could a performance change in
`racket/stream` close that gap?

## Where the time goes (Racket 9.3)

1. `racket/private/for.rkt`: `stream-first` and `stream-rest` each test
   `stream?` (which first tries `list?`) and call `stream-empty?`, which
   tests `stream?` again before dispatching through `prop:stream`;
   `stream-rest` also tests `stream?` on its result. A walk does about six
   `stream?` tests and four property dispatches per node.
2. `racket/private/stream-cons.rkt`: a lazy tail is a wrapper object. After
   it has been forced, every operation on it still goes through
   `stream-force` and then dispatches again on the forced value. The rest
   of a PAUSE node is such a wrapper.

## Measurements

`stream-ops.rkt` (10⁶ nodes):

| | racket/stream public ops | the extension's copy, public ops | peek extension |
| --- | ---: | ---: | ---: |
| walk ready nodes | 342–405 ms | 301 ms | 176–192 ms |
| force a chain of suspensions | 247–291 ms | — | 62–72 ms |

The copy's public operations are as slow as `racket/stream`'s, so the
extension is fast because it reads the structures directly, not because
of anything in its semantics.

`generic-ops-model.rkt` reproduces layer 1 over a local `prop:stream`
and a restructured, equivalent version that looks the property up once
and tests emptiness once per call (same results and error messages):

| | per 10⁶ nodes |
| --- | ---: |
| as in `for.rkt` | 142–166 ms |
| restructured | 100–112 ms |

## Conclusion

Restructuring layer 1 is safe and gains about 30% there, perhaps 15–25%
end to end. Layer 2 remains, and the public generic interface cannot
read the structures directly, so the gap to the peek extension would
stay. Judged not worth a maintainer's review on its own.

The `*-orig`, `*-stream` and `*-stream-pause` files are the single-process
timing scripts behind the table in `../README.md`.
