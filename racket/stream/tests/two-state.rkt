#lang racket/base

(require rackunit racket/match
         (prefix-in stock: racket/stream)
         (prefix-in new: racket/stream/two-state))

(define calls 0)
(define inner
  (new:stream-lazy
   (begin
     (set! calls (add1 calls))
     (new:stream-cons #:eager 'a #:eager new:empty-stream))))
(define outer
  (new:stream-lazy (begin (set! calls (add1 calls)) inner)))
(check-eq? (new:stream-inspect outer) 'lazy)
(check-equal? calls 0)
(check-eq? (new:stream-force-one outer) inner)
(check-equal? calls 1)
(check-eq? (new:stream-inspect outer) 'lazy)
(check-equal? calls 1)
;; Existing generic consumers finish partially evaluated streams.
(check-equal? (stock:stream->list outer) '(a))
(check-equal? calls 2)
(check-eq? (new:stream-inspect outer) 'ready)
(check-eq? (new:stream-force outer) (new:stream-force inner))
(check-equal? (stock:stream->list (new:stream-force-one outer)) '(a))
(check-equal? calls 2)

(define head-calls 0)
(define tail-calls 0)
(define cell
  (new:stream-cons (begin (set! head-calls (add1 head-calls)) 'head)
                   (begin (set! tail-calls (add1 tail-calls)) new:empty-stream)))
(check-eq? (new:stream-inspect cell) 'head-lazy)
(check-eq? (new:stream-force-one cell) cell)
(check-equal? head-calls 0)
(check-equal? tail-calls 0)
(check-equal? (stock:stream-first cell) 'head)
(check-eq? (new:stream-inspect cell) 'ready)
(check-equal? head-calls 1)
(define tail (stock:stream-rest cell))
(check-eq? (new:stream-inspect tail) 'lazy)
(check-equal? tail-calls 0)
(check-eq? (new:stream-force-one tail) new:empty-stream)
(check-equal? tail-calls 1)
(check-eq? (new:stream-inspect tail) 'empty)
(check-eq? (new:stream-inspect '()) 'empty)
(check-eq? (new:stream-inspect '(1 2)) 'ready)
(check-eq? (new:stream-inspect (stock:stream-lazy '())) 'unknown)
(define literal '(1 2))
(check-eq? (new:stream-force-one literal) literal)
(check-equal?
 (call-with-values
  (lambda () (stock:stream-first
              (new:stream-cons (values 1 2) new:empty-stream))) list)
 '(1 2))

;; The stock reentrant/broken guard is retained, including after single steps.
(define self (new:stream-lazy self))
(check-exn #rx"reentrant or broken" (lambda () (new:stream-force-one self)))
(check-eq? (new:stream-inspect self) 'blocked)
(check-exn #rx"reentrant or broken" (lambda () (new:stream-force self)))
(define a (new:stream-lazy b))
(define b (new:stream-lazy a))
(check-eq? (new:stream-force-one a) b)
(check-exn #rx"reentrant or broken" (lambda () (new:stream-force-one b)))
(check-exn #rx"reentrant or broken" (lambda () (new:stream-force a)))
(define broken (new:stream-lazy 42))
(check-exn #rx"non-stream" (lambda () (new:stream-force-one broken)))
(check-exn #rx"reentrant or broken" (lambda () (stock:stream-first broken)))
(define failed (new:stream-lazy (error 'body "failed")))
(check-exn #rx"failed" (lambda () (new:stream-force-one failed)))
(check-exn #rx"reentrant or broken" (lambda () (new:stream-force failed)))

;; Pure finite expressions: stock values versus full forcing after a random
;; prefix of single steps, including shared cached forwarding links.
(define-syntax-rule (define-interpreter name lazy-form cons-stream empty-value)
  (define (name expression)
    (match expression
      ['empty empty-value]
      [`(delay ,body) (lazy-form (name body))]
      [`(eager ,n ,rest) (cons-stream #:eager n #:eager (name rest))]
      [`(lazy ,n ,rest) (cons-stream n (name rest))])))
(define-interpreter original stock:stream-lazy stock:stream-cons stock:empty-stream)
(define-interpreter candidate new:stream-lazy new:stream-cons new:empty-stream)
(define (generate depth)
  (if (zero? depth) 'empty
      (case (random 4)
        [(0) 'empty]
        [(1) `(delay ,(generate (sub1 depth)))]
        [(2) `(eager ,(random 100) ,(generate (sub1 depth)))]
        [else `(lazy ,(random 100) ,(generate (sub1 depth)))])))
(parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
  (random-seed 20261107)
  (for ([i (in-range 500)])
    (define expression (generate 12))
    (define source (candidate expression))
    (define cursor source)
    (for ([j (in-range (random 5))])
      (set! cursor (new:stream-force-one cursor)))
    (check-equal? (stock:stream->list source)
                  (stock:stream->list (original expression)))
    (check-equal? (stock:stream->list cursor) (stock:stream->list source))
    (check-eq? (new:stream-force source) (new:stream-force source))))
