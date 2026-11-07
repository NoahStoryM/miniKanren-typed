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
;; and expected answers unchanged, adapting only those observations here.
(require (prefix-in native: private/racket/stream)
         (for-syntax racket/base))

(define (view-pair? value)
  (or (pair? value)
      (and (native:stream? value) (native:stream-peek-first value) #t)))
(define (view-car value)
  (if (pair? value) (car value) (car (native:stream-peek-first value))))
(define (view-cdr value)
  (if (pair? value) (cdr value) (native:stream-peek-rest value)))
(define (pending-stream? value)
  (and (native:stream? value) (not (native:stream-peek value))))
(define (view-procedure? value)
  (or (procedure? value) (pending-stream? value)))
(define (view-assert value predicate)
  (unless (predicate value) (error 'view-assert "unexpected shape: ~e" value))
  (if (pending-stream? value)
      (lambda () (native:stream-force-once value))
      value))
(define-syntax-rule (view-check-equal? expression expected)
  (let ([actual expression])
    (check-equal? (if (and (native:stream? actual) (not (list? actual)))
                      (native:stream->list actual) actual)
                  expected)))
(let-syntax ([lambda (syntax-rules ()
                      [(_ () body ...) (native:stream-lazy (let () body ...))]
                      [(_ args body ...) (λ args body ...)])])
  (let ([pair? view-pair?] [car view-car] [cdr view-cdr]
        [procedure? view-procedure?] [assert view-assert])
    (let-syntax ([check-equal? (make-rename-transformer #'view-check-equal?)])
      (include "../../../../typed/miniKanren/trs2/tests/ch10.rktl"))))
