#lang typed/racket/no-check

(require racket/include
         (except-in racket/match ==)
         (except-in rackunit fail)
         (only-in racket/list make-list)
         "../main.rkt"
         (prefix-in native: racket/stream)
         (prefix-in search: experiments/search-stream/main)
         (for-syntax racket/base))

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
;; Chapter 10 directly inspects pair/thunk representation. Translate those
;; observers, not its expected answers or the algorithms being tested.
(define (view-pair? value)
  (or (pair? value) (search:stream-head-forced? value)))
(define (view-car value)
  (if (search:stream-head-forced? value) (native:stream-first value) (car value)))
(define (view-cdr value)
  (if (search:stream-head-forced? value) (search:stream-rest/raw value) (cdr value)))
(define (view-procedure? value)
  (or (procedure? value) (search:stream-suspension? value)))
(define (view-assert value predicate)
  (unless (predicate value) (error 'view-assert "unexpected shape: ~e" value))
  (if (search:stream-suspension? value)
      (lambda () (search:stream-force-one value))
      value))
(define-syntax-rule (view-check-equal? expression expected)
  (let ([actual expression])
    (check-equal? (if (and (native:stream? actual) (not (list? actual)))
                      (native:stream->list actual) actual)
                  expected)))
(let-syntax ([lambda (syntax-rules ()
                      [(_ () body ...) (search:stream-suspend body ...)]
                      [(_ args body ...) (λ args body ...)])])
  (let ([pair? view-pair?] [car view-car] [cdr view-cdr]
        [procedure? view-procedure?] [assert view-assert])
    (let-syntax ([check-equal? (make-rename-transformer #'view-check-equal?)])
      (include "../../../../typed/miniKanren/trs2/tests/ch10.rktl"))))
