#lang typed/racket/no-check
(require racket/include rackunit private/racket/stream
         "../main.rkt" "../private/native-stream.rkt"
         (for-syntax racket/base))
(define $car stream-first)
(define ($cdr source)
  (define rest (stream-rest source))
  (if (stream-empty/known? rest) '() rest))
(define $force stream-force-once)

;; The reference programs spell suspension as (λ () ...). Adapt only that
;; constructor, leaving the shared programs and assertions unchanged.
(let-syntax ([λ (syntax-rules ()
                 [(_ () body ...) (suspend-stream (lambda () body ...))]
                 [(_ args body ...) (lambda args body ...)])])
  (include "../../../typed/microKanren/tests/programs.rktl")
  (include "../../../typed/microKanren/tests/core.rktl")
  (include "../../../typed/microKanren/tests/wrappers.rktl"))
