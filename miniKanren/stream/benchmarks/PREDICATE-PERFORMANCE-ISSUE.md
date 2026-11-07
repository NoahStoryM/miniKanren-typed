**What version of Racket are you using?**

9.3 [cs], with typed-racket-lib 1.15 from the 9.3 distribution.

**What program did you run?**

```racket
#lang typed/racket/base

(define-type Tree (U Symbol Null (Pairof Tree Tree)))
(define-predicate generated? Tree)

(: handwritten? (-> Any Boolean : Tree))
(define (handwritten? x)
  (or (symbol? x)
      (null? x)
      (and (pair? x)
           (handwritten? (car x))
           (handwritten? (cdr x)))))

(define input (read (open-input-string "(a b c d e f g h i j k l)")))

(: bench (-> (-> Any Boolean) Void))
(define (bench p)
  (for ([i (in-range 10000)]) (unless (p input) (error "wrong result")))
  (collect-garbage)
  (collect-garbage)
  (time
   (for ([i (in-range 1000000)])
     (unless (p input) (error "wrong result")))))

(for ([i (in-range 3)])
  (displayln 'generated)
  (bench generated?)
  (displayln 'handwritten)
  (bench handwritten?))
```

I compiled the file before running it:

```sh
raco make repro.rkt
racket repro.rkt
```

Output on my machine (times in milliseconds):

```text
generated
cpu time: 3794 real time: 3794 gc time: 15
handwritten
cpu time: 26 real time: 26 gc time: 0
generated
cpu time: 3701 real time: 3701 gc time: 13
handwritten
cpu time: 27 real time: 27 gc time: 0
generated
cpu time: 3761 real time: 3761 gc time: 13
handwritten
cpu time: 25 real time: 25 gc time: 0
```

**What should have happened?**

Both predicates return the expected result, but I was surprised by the size of the performance difference: approximately 3.7–3.8 seconds for the generated predicate versus 25–27 milliseconds for the handwritten one.

I understand that predicate generation may involve generic contract machinery and does not promise identical performance to handwritten code. Is this difference expected for such a small recursive union, or could the generated predicate be optimized?

**If you got an error message, please include it here.**

No error message.

**Please include any other relevant details**

- Linux 6.18.57, x86_64.
- Both predicates and the benchmark are in one fully typed module, with no typed/untyped module boundary.
- Each measurement checks the same 12-element list 1,000,000 times. The list is read at run time rather than constructed as a compile-time constant.
- Each invocation warms up the predicate with 10,000 calls and performs garbage collection before starting the timer.
- Compilation, type checking, and module loading are outside the timed region.
- GC accounts for only 13–15 ms of the generated predicate's measured time in this run.
