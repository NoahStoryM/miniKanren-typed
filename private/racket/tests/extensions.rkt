#lang racket/base
(require rackunit racket/match racket/contract
         private/racket/stream (prefix-in stock: racket/stream))

;; No false-value ambiguity: cached heads are lists of all produced values.
(for ([values-list (in-list (list '() '(#f) '(1 2) '((1 2))))])
  (define source (stream-cons (apply values values-list) empty-stream))
  (check-false (stream-peek-first source))
  (check-equal? (call-with-values (lambda () (stream-first source)) list) values-list)
  (check-equal? (stream-peek-first source) values-list))
(define proc (lambda () 'answer))
(check-equal? (stream-peek-first (stream-cons #:eager proc #:eager empty-stream)) (list proc))
(check-eq? (stream-peek empty-stream) empty-stream)
(check-false (stream-peek-first empty-stream))
(check-false (stream-peek-rest empty-stream))
(check-equal? (stream-peek-first '(#f 2)) '(#f))
(check-equal? (stream-peek-rest '(1 2)) '(2))
(check-equal? (stream-peek-rest '(1)) '())

;; Rest means the stored tail object, not the last element or a forced suffix.
(define head-calls 0)
(define tail-calls 0)
(define cell
  (stream-cons (begin (set! head-calls (add1 head-calls)) 'head)
               (begin (set! tail-calls (add1 tail-calls)) empty-stream)))
(check-eq? (stream-peek cell) cell)
(check-false (stream-peek-first cell))
(define tail (stream-peek-rest cell))
(check-true (stream? tail))
(check-eq? tail (stream-rest cell))
(check-false (stream-peek tail))
(check-equal? (list head-calls tail-calls) '(0 0))
(check-eq? (stream-force-once tail) empty-stream)
(check-eq? (stream-peek tail) empty-stream)
(check-eq? (stream-peek-rest cell) tail)
(check-equal? (list head-calls tail-calls) '(0 1))

;; Cache peeks are one-hop; first/rest peeks can follow existing cached links.
(define calls 0)
(define ready (stream-cons #:eager 'a #:eager empty-stream))
(define inner (stream-lazy (begin (set! calls (add1 calls)) ready)))
(define outer (stream-lazy (begin (set! calls (add1 calls)) inner)))
(check-false (stream-peek outer))
(check-false (stream-peek-first outer))
(check-false (stream-peek-rest outer))
(check-equal? calls 0)
(check-eq? (stream-force-once outer) inner)
(check-eq? (stream-peek outer) inner)
(check-false (stream-peek inner))
(check-equal? calls 1)
(check-eq? (stream-force-once inner) ready)
(check-equal? (stream-peek-first outer) '(a))
(check-eq? (stream-peek-rest outer) empty-stream)
(check-eq? (stream-peek outer) inner) ; the peeks did not compress/mutate
(check-equal? calls 2)
(check-eq? (stream-force-once outer) ready)
(check-eq? (stream-peek outer) ready) ; stepping compresses old links
(check-equal? calls 2)
(check-eq? (stream-force outer) ready)

;; All copied derived constructors participate; no old lexical bindings escape.
(for ([source (in-list
               (list (stream 1 2)
                     (stream* 1 (stream 2))
                     (stream-map add1 (stream 0 1))
                     (stream-filter positive? (stream -1 1 2))
                     (for/stream ([i '(1 2)]) i)))])
  (check-true (stream? (stream-force-once source)))
  (check-equal? (stream->list source) '(1 2))
  (check-equal? (stream-peek-first source) '(1)))

;; Foreign and contracted streams must not run methods or bypass contracts.
(struct opaque ()
  #:property prop:stream
  (vector (lambda (_) (error 'opaque "empty called"))
          (lambda (_) (error 'opaque "first called"))
          (lambda (_) (error 'opaque "rest called"))))
(define foreign (opaque))
(check-eq? (stream-peek foreign) foreign)
(check-false (stream-peek-first foreign))
(check-false (stream-peek-rest foreign))
(check-eq? (stream-force-once foreign) foreign)
(define guarded
  (contract (stream/c integer?)
            (stream-cons #:eager 'bad #:eager empty-stream) 'provider 'client))
(check-false (stream-peek guarded))
(check-false (stream-peek-first guarded))
(check-false (stream-peek-rest guarded))
(check-eq? (stream-force-once guarded) guarded)
(check-exn exn:fail:contract? (lambda () (stream-first guarded)))

;; Repeated old-root steps progress once, compress, and retain usable aliases.
(define root-calls 0)
(define (chain n)
  (if (zero? n) ready
      (stream-lazy (begin (set! root-calls (add1 root-calls)) (chain (sub1 n))))))
(define root (chain 200))
(define alias #f)
(for ([i (in-range 200)])
  (define result (stream-force-once root))
  (when (= i 50) (set! alias result))
  (check-equal? root-calls (add1 i))
  (check-eq? (stream-peek root) result))
(check-equal? (stream-peek-first root) '(a))
(check-equal? (stream->list alias) '(a))
(check-equal? root-calls 200)

(define self (stream-lazy self))
(check-exn #rx"reentrant or broken" (lambda () (stream-force-once self)))
(check-false (stream-peek self))
(check-exn #rx"reentrant or broken" (lambda () (stream-force self)))
(define a (stream-lazy b))
(define b (stream-lazy a))
(check-eq? (stream-force-once a) b)
(check-exn #rx"reentrant or broken" (lambda () (stream-force-once b)))
(check-exn #rx"reentrant or broken" (lambda () (stream-force a)))
(define wrapped-self
  (stream-lazy (contract (stream/c any/c) wrapped-self 'provider 'client)))
(check-exn #rx"reentrant or broken" (lambda () (stream-force-once wrapped-self)))
(define broken (stream-lazy 42))
(check-exn #rx"non-stream" (lambda () (stream-force-once broken)))
(check-exn #rx"reentrant or broken" (lambda () (stream-force-once broken)))
(for ([operation (in-list (list stream-peek stream-peek-first stream-peek-rest stream-force-once))])
  (check-exn exn:fail:contract? (lambda () (operation 42)))
  (check-exn exn:fail:contract? (lambda () (operation '(1 . 2)))))

;; Seeded finite streams agree with stock after partial native forcing.
(define-syntax-rule (define-interpreter name lazy-form cons-form empty-value)
  (define (name expression)
    (match expression
      ['empty empty-value]
      [`(delay ,body) (lazy-form (name body))]
      [`(cell ,eager? ,n ,rest)
       (if eager? (cons-form #:eager n #:eager (name rest))
           (cons-form n (name rest)))])))
(define-interpreter original stock:stream-lazy stock:stream-cons stock:empty-stream)
(define-interpreter candidate stream-lazy stream-cons empty-stream)
(define (generate depth)
  (if (zero? depth) 'empty
      (case (random 4)
        [(0) 'empty]
        [(1) `(delay ,(generate (sub1 depth)))]
        [else `(cell ,(zero? (random 2)) ,(random 100) ,(generate (sub1 depth)))])))
(parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
  (random-seed 20261107)
  (for ([i (in-range 100)])
    (define expression (generate 10))
    (define source (candidate expression))
    (define cursor source)
    (for ([j (in-range (random 5))]) (set! cursor (stream-force-once cursor)))
    (check-equal? (stream->list source) (stock:stream->list (original expression)))
    (check-equal? (stream->list cursor) (stream->list source))))

;; The public delta is exactly the four proposed operations.
(define (export-names path)
  (define-values (values syntax) (module->exports path))
  (sort (apply append
               (for/list ([phases (in-list (list values syntax))])
                 (define entry (assv 0 phases))
                 (if entry (map car (cdr entry)) '()))) symbol<?))
(check-equal? (export-names 'private/racket/stream)
              (sort (append (export-names 'racket/stream)
                            '(stream-peek stream-peek-first stream-peek-rest stream-force-once))
                    symbol<?))
