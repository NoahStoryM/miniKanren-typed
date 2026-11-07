#lang racket/base

(module stock racket/base
  (require racket/include racket/stream)
  (include "workloads.rktl"))

(module fork racket/base
  (require racket/include "../two-state.rkt")
  (include "workloads.rktl"))

(require json racket/match
         (prefix-in stock: (submod "." stock))
         (prefix-in fork: (submod "." fork)))

(module+ main
  (match-define (list backend workload count rounds)
    (vector->list (current-command-line-arguments)))
  (define variant (string->symbol backend))
  (define kind (string->symbol workload))
  (define n (string->number count))
  (define repetitions (string->number rounds))
  (define memory? (memq kind '(memory-chain memory-tail)))
  (define actual-kind
    (case kind [(memory-chain) 'delay-chain] [(memory-tail) 'lazy-tail] [else kind]))
  (define exercise (if (eq? variant 'stock) stock:exercise fork:exercise))
  (define memory (if (eq? variant 'stock) stock:retained-memory fork:retained-memory))
  ;; One small and one full warmup; no module-load/compilation time is timed.
  (void (exercise actual-kind (max 1 (quotient n 10))))
  (void (exercise actual-kind n))
  (when memory? (void (memory actual-kind n)))
  (define samples
    (for/list ([i (in-range repetitions)])
      (collect-garbage) (collect-garbage) (collect-garbage)
      (if memory?
          (hasheq 'retained_bytes (memory actual-kind n))
          (let-values ([(result cpu real gc) (time-apply exercise (list kind n))])
            (hasheq 'cpu_ms cpu 'real_ms real 'gc_ms gc 'checksum (car result))))))
  (write-json (hasheq 'backend backend 'workload workload 'n n
                      'racket_version (version) 'samples samples))
  (newline))
