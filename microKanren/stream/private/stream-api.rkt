#lang racket/base

(require racket/match)
(provide (all-defined-out))

;; Representation interface. inspect must not resume a suspension.
;; empty   : S A
;; emit    : A * S A -> S A       (strict construction; tail may be paused)
;; suspend : (() -> S A) -> S A   (does not call its argument)
;; inspect : S A -> End | Ready(A, S A) | Paused(() -> S A)
(struct representation (empty emit suspend inspect) #:transparent)
(struct end () #:transparent)
(struct ready (value rest) #:transparent)
(struct paused (resume) #:transparent)

;; The search algebra and its observers. Only make-stream-api below
;; knows how scheduling is defined; it does not know the representation.
(struct stream-api
  (empty unit suspend merge bind inspect pull take take-all trace)
  #:transparent)

;; A syntax convenience, not an additional semantic operation.
(define-syntax-rule (suspend/with api body ...)
  ((stream-api-suspend api) (λ () body ...)))

(define (make-stream-api rep)
  (match-define (representation empty emit suspend inspect) rep)

  (define (unit value) (emit value empty))

  ;; Keep the left operand after an answer; exchange operands at a pause.
  ;; Racket evaluates the recursive mature tail before emit is called.
  (define (merge left right)
    (match (inspect left)
      [(end) right]
      [(ready value rest) (emit value (merge rest right))]
      [(paused resume) (suspend (λ () (merge right (resume))))]))

  (define (bind source next)
    (match (inspect source)
      [(end) empty]
      [(ready value rest) (merge (next value) (bind rest next))]
      [(paused resume) (suspend (λ () (bind (resume) next)))]))

  ;; Unlike inspect, pull may cross arbitrarily many pauses.
  (define (pull source)
    (match (inspect source)
      [(paused resume) (pull (resume))]
      [_ source]))

  (define (take n source)
    (cond
      [(zero? n) '()]
      [else
       (match (inspect source)
         [(end) '()]
         [(ready value rest) (cons value (take (sub1 n) rest))]
         [(paused resume) (take n (resume))])]))

  (define (take-all source)
    (match (inspect source)
      [(end) '()]
      [(ready value rest) (cons value (take-all rest))]
      [(paused resume) (take-all (resume))]))

  ;; Include pause positions; do not evaluate past the requested prefix.
  (define (trace n source [project values])
    (cond
      [(zero? n) '()]
      [else
       (match (inspect source)
         [(end) '(end)]
         [(ready value rest)
          (cons (list 'answer (project value))
                (if (= n 1) '() (trace (sub1 n) rest project)))]
         [(paused resume)
          (cons 'pause
                (if (= n 1) '() (trace (sub1 n) (resume) project)))])]))

  (stream-api empty unit suspend merge bind inspect pull take take-all trace))
