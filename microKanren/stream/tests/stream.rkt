#lang racket/base

(require rackunit racket/match racket/promise racket/stream
         (prefix-in search: "../stream.rkt"))

(define calls 0)
(define source
  (search:stream-suspend
   (set! calls (add1 calls))
   (search:stream-suspend
    (set! calls (add1 calls))
    (search:stream-unit 'answer))))
(check-true (stream? source))
(check-true (search:paused? (search:stream-inspect source)))
(check-equal? calls 0)
(check-equal? (search:stream-take 0 source) '())
(check-equal? calls 0)
(define next (search:stream-force-one source))
(check-equal? calls 1)
(check-true (search:stream-suspension? next))
(check-eq? (search:stream-force-one source) next)
(check-equal? calls 1)
(check-equal? (stream->list source) '(answer))
(check-equal? calls 2)
(check-true (search:stream-suspension? source))
(check-eq? (search:stream-force-one source) next)
(check-equal? (search:stream-trace 10 source)
              '(pause pause (answer answer) end))
(check-equal? calls 2)

;; Logical pause identity is independent of its current cached result.
(define warmed-first
  (search:stream-suspend (search:stream-unit 'left)))
(check-equal? (stream->list warmed-first) '(left))
(check-equal?
 (search:stream-trace 10
  (search:stream-merge warmed-first (search:stream-unit 'right)))
 '(pause (answer right) (answer left) end))

;; Unit's answer is already available; a promise answer remains untouched.
(define answer (delay (error 'answer "should remain an answer")))
(define single (search:stream-unit answer))
(check-true (search:stream-head-forced? single))
(check-eq? (stream-first single) answer)
(check-false (promise-forced? answer))
(check-equal? (search:stream-rest/raw single) '())

;; Private state queries read native fields without running either expression.
(define head-calls 0)
(define tail-calls 0)
(define foreign
  (stream-cons (begin (set! head-calls (add1 head-calls)) 'head)
               (begin (set! tail-calls (add1 tail-calls)) empty-stream)))
(check-false (search:stream-head-forced? foreign))
(check-equal? head-calls 0)
(check-equal? tail-calls 0)
(check-exn #rx"experimental constructors" (lambda () (search:stream-inspect foreign)))
(check-equal? (stream-first foreign) 'head)
(check-true (search:stream-head-forced? foreign))
(check-equal? head-calls 1)
(check-equal? tail-calls 0)
;; A foreign lazy tail has no declared search suspension boundary.
(check-exn #rx"experimental constructors"
           (lambda () (search:stream-inspect (search:stream-rest/raw foreign))))
(check-equal? tail-calls 0)
(check-true (stream-empty? (stream-rest foreign)))
(check-equal? tail-calls 1)

;; A fresh native delayed stream must not be silently forced by inspect.
(define foreign-calls 0)
(define delayed
  (stream-lazy (begin (set! foreign-calls (add1 foreign-calls)) empty-stream)))
(check-exn #rx"experimental constructors" (lambda () (search:stream-inspect delayed)))
(check-equal? foreign-calls 0)
