#lang racket/base
;; Run: racket miniKanren/stream-pause/benchmarks/stream-ops.rkt
;; Cost of walking 1e6 nodes with racket/stream vs the peek extension.
(require (prefix-in r: racket/stream) (prefix-in p: private/racket/stream))
(define N 1000000)
(define (build-r n) (if (zero? n) r:empty-stream (r:stream-cons #:eager n #:eager (build-r (sub1 n)))))
(define (build-p n) (if (zero? n) p:empty-stream (p:stream-cons #:eager n #:eager (build-p (sub1 n)))))
(define (walk-r s acc) (if (r:stream-empty? s) acc (walk-r (r:stream-rest s) (+ acc (r:stream-first s)))))
(define (walk-p s acc) (if (eq? (p:stream-peek s) p:empty-stream) acc
                           (walk-p (p:stream-peek-rest s) (+ acc (car (p:stream-peek-first s))))))
(define (lazy-r n) (if (zero? n) r:empty-stream (r:stream-cons #:eager 0 (lazy-r (sub1 n)))))
(define (lazy-p n) (if (zero? n) p:empty-stream (p:stream-lazy (lazy-p (sub1 n)))))
(define (force-r s k) (if (r:stream-empty? s) k (force-r (r:stream-rest s) (add1 k))))
(define (force-p s k) (if (eq? (p:stream-peek s) p:empty-stream) k
                          (let ([c (p:stream-peek s)]) (force-p (or c (p:stream-force-once s)) (add1 k)))))
(define-syntax-rule (t name e) (begin (collect-garbage) (let ([t0 (current-process-milliseconds)]) e (printf "~a: ~a ms\n" name (- (current-process-milliseconds) t0)))))
(t "racket/stream ready walk" (walk-r (build-r N) 0))
(t "peek ext      ready walk" (walk-p (build-p N) 0))
(t "racket/stream PAUSE chain" (force-r (lazy-r N) 0))
(t "peek ext      lazy chain " (force-p (lazy-p N) 0))
(define (walk-pg s acc) (if (p:stream-empty? s) acc (walk-pg (p:stream-rest s) (+ acc (p:stream-first s)))))
(t "copy's public ops ready walk" (walk-pg (build-p N) 0))
