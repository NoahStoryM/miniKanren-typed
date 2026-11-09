# Kanren ports on `racket/stream` with explicit pauses

```racket
(require microKanren/stream-pause)       ; μKanren
(require miniKanren/stream-pause/trs1)   ; first edition
(require miniKanren/stream-pause/trs2)   ; second edition
```

Research variant of `miniKanren/stream` (ambKanren issue #14). The
`stream` ports need four operations beyond `racket/stream`
(`stream-lazy`, `stream-force-once`, `stream-peek`, `stream-peek-first` /
`-rest`) to tell a suspension from a ready node without forcing it. Here
a suspension is instead an explicit element:

| Original | `stream` port | `stream-pause` port |
| --- | --- | --- |
| Empty stream | `empty-stream` | `empty-stream` |
| Ready pair | `stream-cons #:eager a #:eager rest` | same |
| Suspension `(λ () e)` | `(stream-lazy e)` | `(stream-cons #:eager PAUSE e)` |
| Calling a suspension | `stream-force-once` | `stream-rest` of a PAUSE node |
| Is it ready / empty / a suspension? | `stream-peek`, `stream-peek-first` | `stream-empty?`, then `(eq? (stream-first s) PAUSE)` |

`PAUSE`, `suspend` and `paused?` are in `private/stream-pause`. Only
public `racket/stream` operations are used; no stream extension is
needed. Every search stream has an evaluated head, so the case analysis
never forces a suspension. Goals must not return a stream whose head is
itself delayed.

TRS1 needs no extra tag either: `unit` is a ready node followed by
`empty-stream`, `choice` a ready node followed by a PAUSE node, and
`case∞` looks only at the head of that tail.

## Differences from thunks

- The suspended expression runs when the rest of the PAUSE node is first
  inspected, which can be later than the original call. Answers and
  pauses come in the same order.
- The rest is memoized, but a forced PAUSE node is still a PAUSE node.
  A reused stream therefore schedules the same way on every traversal,
  unlike the `stream` port, whose interleaving can depend on cache state.

## Tests

| Suite | Result |
| --- | --- |
| `microKanren/stream-pause/tests/main.rkt` (paper programs) | 20 passed |
| `miniKanren/stream-pause/trs1/tests/main.rkt` (book, ch1–10) | 254 passed |
| `miniKanren/stream-pause/trs2/tests/main.rkt` (book, ch1–10) | 331 passed |
| `microKanren/stream-pause/tests/scheduling.rkt` | 645 passed |

All book checks are strict, as in the originals. Chapter 10 of TRS2
observes the pair/thunk representation; its fixtures are kept and only
the observers are adapted (a PAUSE node is viewed as a thunk).
`scheduling.rkt` compares the answer/pause trace of μKanren and TRS2
merges and binds with the original pair/thunk implementations on 250
random stream expressions, plus TRS1's four operators, and checks that
ready answers do not run their tails, that `ifte` / `once` keep leading
pauses, and that reuse does not change scheduling.

## Performance

Racket 9.3 CS, Linux x86_64, CPU time of one run after a warm-up
(single process, indicative only):

| Workload | original (typed) | `stream` (peek) | `stream-pause` |
| --- | ---: | ---: | ---: |
| μKanren, 10⁶ answers | 417 ms | 1710 ms | 5611 ms |
| TRS1 `conde`, 10⁶ answers | 416 ms | 779 ms | 1340 ms |
| TRS1 `condi`, 10⁶ answers | 499 ms | 659 ms | 1468 ms |
| TRS2, 10⁶ answers | 431 ms | 526 ms | 1654 ms |
| TRS2 `very-recursiveo`, 10⁵ answers | 184 ms | 289 ms | 1543 ms |

Most of the gap to the `stream` port is the cost of the public stream
operations themselves: walking 10⁶ ready nodes takes 405 ms with
`racket/stream` against 192 ms with the extension, and forcing a chain
of 10⁶ suspensions 291 ms against 72 ms. Reading the head once per node
instead of twice gains about 25% in TRS2; the ports keep the clearer
three-way `cond`.
