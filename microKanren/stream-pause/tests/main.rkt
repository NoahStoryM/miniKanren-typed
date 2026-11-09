#lang typed/racket/no-check

(require racket/include
         rackunit
         racket/stream
         private/stream-pause
         "../main.rkt")

;; Only the fixture's representation observers and explicit thunks change.
(define ($car $) (stream-first $))
(define ($cdr $)
  (let ([$ (stream-rest $)])
    (if (stream-empty? $) '() $)))
(define ($force $) (stream-rest $))

(let-syntax ([λ (syntax-rules ()
                 [(_ () body ...) (suspend (let () body ...))]
                 [(_ args body ...) (λ args body ...)])])
  (include "../../../typed/microKanren/tests/programs.rktl")
  (include "../../../typed/microKanren/tests/core.rktl")
  (include "../../../typed/microKanren/tests/wrappers.rktl"))
