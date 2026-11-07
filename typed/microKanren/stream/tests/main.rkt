#lang typed/racket/no-check
(require racket/include rackunit racket/stream
         "../main.rkt" racket/stream/private/representation
         (for-syntax racket/base))
(define $car stream-first)
(define $cdr stream-rest/raw)
(define $force stream-force-one)

;; The reference programs spell suspension as (λ () ...). Adapt only that
;; constructor, leaving the shared programs and assertions unchanged.
(let-syntax ([λ (syntax-rules ()
                 [(_ () body ...) (suspend-stream (lambda () body ...))]
                 [(_ args body ...) (lambda args body ...)])])
  (include "../../tests/programs.rktl")
  (include "../../tests/core.rktl")
  (include "../../tests/wrappers.rktl"))
