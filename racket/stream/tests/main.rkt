#lang racket/base

(require rackunit racket/match
         (prefix-in stock: racket/stream)
         (prefix-in new: racket/stream/main))

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

;; Repeated steps from the same old root still execute exactly one new body.
(define root-calls 0)
(define (counted-chain n)
  (if (zero? n) (new:stream-cons #:eager 'done #:eager new:empty-stream)
      (new:stream-lazy
       (begin (set! root-calls (add1 root-calls))
              (counted-chain (sub1 n))))))
(define old-root (counted-chain 100))
(for ([i (in-range 100)])
  (define result (new:stream-force-one old-root))
  (check-equal? root-calls (add1 i))
  (check-eq? (new:stream-inspect result) (if (= i 99) 'ready 'lazy)))
(check-eq? (new:stream-inspect old-root) 'ready)
(check-equal? (stock:stream->list old-root) '(done))
(void (new:stream-force-one old-root))
(check-equal? root-calls 100)

;; An alias retained in the middle remains usable after another root compresses.
(define shared-tail (new:stream-lazy (new:stream-lazy '(answer))))
(define shared-root (new:stream-lazy shared-tail))
(check-eq? (new:stream-force-one shared-root) shared-tail)
(define shared-next (new:stream-force-one shared-root))
(check-eq? (new:stream-inspect shared-tail) 'lazy)
(check-equal? (new:stream-force-one shared-root) '(answer))
(check-eq? (new:stream-inspect shared-tail) 'ready)
(check-equal? (stock:stream->list shared-next) '(answer))

;; Compress a long prefix built by advancing a cursor, then resume from its root.
(define prefix-root (counted-chain 100))
(define prefix-cursor
  (for/fold ([cursor prefix-root]) ([i (in-range 50)])
    (new:stream-force-one cursor)))
(check-equal? root-calls 150)
(void (new:stream-force-one prefix-root))
(check-equal? root-calls 151)
(check-eq? (new:stream-inspect prefix-cursor) 'lazy)
(check-equal? (stock:stream->list prefix-root) '(done))
(check-equal? root-calls 200)
(check-equal? (stock:stream->list prefix-cursor) '(done))

;; The facade exports exactly the stock interface plus the two new operations.
(define-values (stock-values stock-syntax) (module->exports 'racket/stream))
(define-values (new-values new-syntax) (module->exports 'racket/stream/main))
(define (phase-zero-names exports)
  (define entry (assv 0 exports))
  (if entry (map car (cdr entry)) '()))
(check-equal? (sort (append (phase-zero-names new-values)
                            (phase-zero-names new-syntax)) symbol<?)
              (sort (append '(stream-inspect stream-force-one)
                            (phase-zero-names stock-values)
                            (phase-zero-names stock-syntax)) symbol<?))
(check-eq? new:stream-map stock:stream-map)
(check-eq? new:stream-first stock:stream-first)
(check-equal? (new:stream->list (new:stream-map add1 (new:stream 1 2 3))) '(2 3 4))
(check-equal? (new:stream->list (new:stream* 1 2 (new:stream 3))) '(1 2 3))
(check-equal? (new:stream->list (new:for/stream ([i (in-range 3)]) i)) '(0 1 2))

;; Preserve the independent eager/lazy choices of the native constructor.
(for ([mode '(lazy head tail both)])
  (define heads 0)
  (define tails 0)
  (define (head) (set! heads (add1 heads)) 7)
  (define (rest) (set! tails (add1 tails)) new:empty-stream)
  (define source
    (case mode
      [(lazy) (new:stream-cons (head) (rest))]
      [(head) (new:stream-cons #:eager (head) (rest))]
      [(tail) (new:stream-cons (head) #:eager (rest))]
      [(both) (new:stream-cons #:eager (head) #:eager (rest))]))
  (check-equal? heads (if (memq mode '(head both)) 1 0))
  (check-equal? tails (if (memq mode '(tail both)) 1 0))
  (check-equal? (new:stream->list source) '(7))
  (check-equal? (list heads tails) '(1 1)))
