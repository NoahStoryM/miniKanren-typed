#lang racket/base
;; Feasibility model for a racket/stream performance issue.
;; Reproduces the generic stream operations of racket/private/for.rkt
;; (v9.3) over a local prop:stream, then a restructured version that
;; looks the property up once and checks emptiness once per call.
(require racket/unsafe/ops)

(define-values (prop:stream stream-via-prop? stream-ref)
  (make-struct-type-property 'stream))

;; ---- as in for.rkt ----
(define (stream? v) (or (list? v) (stream-via-prop? v)))
(define (stream-empty? v)
  (or (null? v)
      (if (stream? v)
          (if (pair? v) #f ((unsafe-vector-ref (stream-ref v) 0) v))
          (raise-argument-error 'stream-empty? "stream?" v))))
(define (unsafe-stream-first v)
  (cond [(pair? v) (car v)] [else ((unsafe-vector-ref (stream-ref v) 1) v)]))
(define (stream-first v)
  (if (and (stream? v) (not (stream-empty? v)))
      (unsafe-stream-first v)
      (raise-argument-error 'stream-first "(and/c stream? (not/c stream-empty?))" v)))
(define (unsafe-stream-rest v)
  (cond [(pair? v) (cdr v)]
        [else (let ([r ((unsafe-vector-ref (stream-ref v) 2) v)])
                (unless (stream? r) (raise-mismatch-error 'stream-rest-guard "result is not a stream: " r))
                r)]))
(define (stream-rest v)
  (if (and (stream? v) (not (stream-empty? v)))
      (unsafe-stream-rest v)
      (raise-argument-error 'stream-rest "(and/c stream? (not/c stream-empty?))" v)))

;; ---- restructured: same results and errors ----
(define (stream-first* v)
  (cond
    [(pair? v) (car v)]
    [(stream-via-prop? v)
     (define m (stream-ref v))
     (if ((unsafe-vector-ref m 0) v)
         (raise-argument-error 'stream-first "(and/c stream? (not/c stream-empty?))" v)
         ((unsafe-vector-ref m 1) v))]
    [else (raise-argument-error 'stream-first "(and/c stream? (not/c stream-empty?))" v)]))
(define (stream-rest* v)
  (cond
    [(pair? v) (cdr v)]
    [(stream-via-prop? v)
     (define m (stream-ref v))
     (if ((unsafe-vector-ref m 0) v)
         (raise-argument-error 'stream-rest "(and/c stream? (not/c stream-empty?))" v)
         (let ([r ((unsafe-vector-ref m 2) v)])
           (unless (or (pair? r) (null? r) (stream-via-prop? r) (list? r))
             (raise-mismatch-error 'stream-rest-guard "result is not a stream: " r))
           r))]
    [else (raise-argument-error 'stream-rest "(and/c stream? (not/c stream-empty?))" v)]))
(define (stream-empty?* v)
  (cond [(null? v) #t] [(pair? v) #f]
        [(stream-via-prop? v) ((unsafe-vector-ref (stream-ref v) 0) v)]
        [else (raise-argument-error 'stream-empty? "stream?" v)]))

;; an eager cons node, like eagerly-created-stream with #:eager fields
(struct node (first rest)
  #:property prop:stream (vector (λ (p) #f) (λ (p) (node-first p)) (λ (p) (node-rest p))))
(struct nil ()
  #:property prop:stream (vector (λ (p) #t) (λ (p) (error 'first)) (λ (p) (error 'rest))))
(define the-nil (nil))

(define N 1000000)
(define (build n) (if (zero? n) the-nil (node n (build (sub1 n)))))
(define s (build N))
(define (walk s acc) (if (stream-empty? s) acc (walk (stream-rest s) (+ acc (stream-first s)))))
(define (walk* s acc) (if (stream-empty?* s) acc (walk* (stream-rest* s) (+ acc (stream-first* s)))))
(define-syntax-rule (t name e)
  (begin (collect-garbage)
         (let ([t0 (current-inexact-milliseconds)])
           (for ([_ 5]) e)
           (printf "~a: ~a ms per 1e6 nodes\n" name (round (/ (- (current-inexact-milliseconds) t0) 5))))))
(t "as in for.rkt   " (walk s 0))
(t "restructured    " (walk* s 0))
(unless (= (walk s 0) (walk* s 0)) (error "mismatch"))
