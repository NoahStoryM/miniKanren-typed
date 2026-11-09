#lang typed/racket/no-check

(require racket/include
         (except-in racket/match ==)
         (except-in rackunit fail)
         (only-in racket/list make-list)
         "../main.rkt")

(include "../../../../typed/miniKanren/trs2/tests/ch1.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch2.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch3.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch4.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch5.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch6.rktl")
(include "../../../../typed/miniKanren/trs2/tests/arith.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch7.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch8.rktl")
(include "../../../../typed/miniKanren/trs2/tests/ch9.rktl")

;; Chapter 10 observes the old pair/thunk representation. Keep its fixtures
;; and expected answers unchanged, adapting only those observations here:
;; a ready node is a pair, a PAUSE node is a thunk whose call is its rest.
(require (prefix-in native: racket/stream)
         private/stream-pause
         (for-syntax racket/base))

(define (non-empty-stream? value)
  (and (native:stream? value) (not (list? value)) (not (native:stream-empty? value))))
(define (pause-node? value)
  (and (non-empty-stream? value) (paused? value)))
(define (view-pair? value)
  (or (pair? value)
      (and (non-empty-stream? value) (not (paused? value)))))
(define (view-car value)
  (if (pair? value) (car value) (native:stream-first value)))
(define (view-cdr value)
  (if (pair? value) (cdr value) (native:stream-rest value)))
(define (view-procedure? value)
  (or (procedure? value) (pause-node? value)))
(define (view-assert value predicate)
  (unless (predicate value) (error 'view-assert "unexpected shape: ~e" value))
  (if (pause-node? value)
      (lambda () (native:stream-rest value))
      value))
;; A mature, finite stream is compared as the list of its answers.
(define (mature->list value)
  (let loop ([s value])
    (cond [(native:stream-empty? s) '()]
          [(paused? s) #f]
          [else (let ([rest (loop (native:stream-rest s))])
                  (and rest (cons (native:stream-first s) rest)))])))
(define-syntax-rule (view-check-equal? expression expected)
  (let ([actual expression])
    (check-equal? (or (and (native:stream? actual) (not (list? actual))
                           (mature->list actual))
                      actual)
                  expected)))
(let-syntax ([lambda (syntax-rules ()
                      [(_ () body ...) (suspend (let () body ...))]
                      [(_ args body ...) (λ args body ...)])])
  (let ([pair? view-pair?] [car view-car] [cdr view-cdr]
        [procedure? view-procedure?] [assert view-assert])
    (let-syntax ([check-equal? (make-rename-transformer #'view-check-equal?)])
      (include "../../../../typed/miniKanren/trs2/tests/ch10.rktl"))))
