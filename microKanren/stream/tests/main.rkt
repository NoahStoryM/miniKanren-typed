#lang typed/racket/no-check

(require racket/include
         rackunit
         "../main.rkt"
         private/racket/stream)

;; Only the fixture's representation observers and explicit thunks change.
(define ($car $) (car (stream-peek-first $)))
(define ($cdr $)
  (let ([$ (stream-peek-rest $)])
    (if (eq? $ empty-stream) '() $)))
(define ($force $) (stream-force-once $))

(let-syntax ([λ (syntax-rules ()
                 [(_ () body ...) (stream-lazy (let () body ...))]
                 [(_ args body ...) (λ args body ...)])])
  (include "../../../typed/microKanren/tests/programs.rktl")
  (include "../../../typed/microKanren/tests/core.rktl")
  (include "../../../typed/microKanren/tests/wrappers.rktl"))
