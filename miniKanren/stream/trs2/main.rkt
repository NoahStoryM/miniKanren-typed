#lang racket/base

;; miniKanren as implemented in The Reasoned Schemer, second edition,
;; using private/racket/stream.
;;
;; Source: https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd
;; (trs2-impl.scm), the code of chapter 10 ("Under the Hood") and
;; appendix A ("Connecting the Wires") of Daniel P. Friedman, William E.
;; Byrd, Oleg Kiselyov, and Jason Hemann, The Reasoned Schemer, second
;; edition, MIT Press, 2018.
;; Copyright (c) 2018 Daniel P. Friedman, William E. Byrd, Oleg Kiselyov,
;; and Jason Hemann.  MIT License; see NOTICE for the full text and for
;; the changes made here.

(require private/racket/stream
         (for-syntax racket/base syntax/parse))

(provide (all-defined-out))


;;; Terms and substitutions


(struct var (name)
  #:transparent)

(define (data? v)
  (or (boolean? v) (number? v) (char? v) (bytes? v) (string? v)
      (keyword? v) (null? v) (symbol? v)
      (and (pair? v) (data? (car v)) (data? (cdr v)))))

(define empty-s '())

(define (walk v s)
  (cond
    [(and (var? v) (assq v s))
     => (λ (a) (walk (cdr a) s))]
    [else v]))

;; Extending with a ground term can never create a cycle, so that case
;; always succeeds.
(define (ext-s x v s)
  (cond
    [(data? v) (cons (cons x v) s)]
    [(occurs? x v s) #f]
    [else (cons (cons x v) s)]))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [(equal? u v) s]
      [else #f])))


;;; Streams

;; Answers and tails are eager; only stream-lazy introduces a suspension.
;; mplus: interleaves by swapping the two streams at each suspension.
(define (append-inf s-inf t-inf)
  (cond
    [(eq? (stream-peek s-inf) empty-stream) t-inf]
    [(stream-peek-first s-inf)
     (stream-cons #:eager (car (stream-peek-first s-inf))
                  #:eager (append-inf (stream-peek-rest s-inf) t-inf))]
    [(stream-peek s-inf) => (λ (s-inf) (append-inf s-inf t-inf))]
    [else (stream-lazy (append-inf t-inf (stream-force-once s-inf)))]))

;; bind, with the arguments in the order of append-map.
(define (append-map-inf g s-inf)
  (cond
    [(eq? (stream-peek s-inf) empty-stream) empty-stream]
    [(stream-peek-first s-inf)
     (append-inf (g (car (stream-peek-first s-inf)))
                 (append-map-inf g (stream-peek-rest s-inf)))]
    [(stream-peek s-inf) => (λ (s-inf) (append-map-inf g s-inf))]
    [else (stream-lazy (append-map-inf g (stream-force-once s-inf)))]))

(define (take-inf n s-inf)
  (cond
    [(and n (zero? n)) '()]
    [(eq? (stream-peek s-inf) empty-stream) '()]
    [(stream-peek-first s-inf)
     (cons (car (stream-peek-first s-inf))
           (take-inf (and n (sub1 n)) (stream-peek-rest s-inf)))]
    [(stream-peek s-inf) => (λ (s-inf) (take-inf n s-inf))]
    [else (take-inf n (stream-force-once s-inf))]))


;;; Goals

(define ((== u v) s)
  (let ([s (unify u v s)])
    (if s (stream-cons #:eager s #:eager empty-stream) empty-stream)))

(define (succeed s) (stream-cons #:eager s #:eager empty-stream))

(define (fail s) empty-stream)

(define ((disj2 g1 g2) s)
  (append-inf (g1 s) (g2 s)))

(define ((conj2 g1 g2) s)
  (append-map-inf g2 (g1 s)))

(define (call/fresh name f)
  (f (var name)))

(define ((ifte g1 g2 g3) s)
  (let loop ([s-inf (g1 s)])
    (cond
      [(eq? (stream-peek s-inf) empty-stream) (g3 s)]
      [(stream-peek-first s-inf) (append-map-inf g2 s-inf)]
      [(stream-peek s-inf) => loop]
      [else (stream-lazy (loop (stream-force-once s-inf)))])))

(define ((once g) s)
  (let loop ([s-inf (g s)])
    (cond
      [(eq? (stream-peek s-inf) empty-stream) empty-stream]
      [(stream-peek-first s-inf)
       (stream-cons #:eager (car (stream-peek-first s-inf)) #:eager empty-stream)]
      [(stream-peek s-inf) => loop]
      [else (stream-lazy (loop (stream-force-once s-inf)))])))


;;; Reification

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s))]
      [else v])))

(define (reify-name n)
  (string->symbol (format "_~a" n)))

;; The names are symbols, hence ground, so this ext-s cannot fail.
(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v) (ext-s v (reify-name (length r)) r)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
      [else r])))

(define ((reify v) s)
  (let* ([v (walk* v s)]
         [r (reify-s v empty-s)])
    (walk* v r)))

(define (run-goal n g)
  (take-inf n (g empty-s)))


;;; Syntax (appendix A)

(define-syntax disj
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g0 g ...) (disj2 g0 (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g0 g ...) (conj2 g0 (conj g ...))]))

;; The body is suspended: this is where streams become immature.
(define-syntax (defrel stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (name:id arg ...) g ...)
     #:with (param ...)
     (for/list ([arg (in-list (syntax->list #'(arg ...)))])
       (syntax-parse arg
         #:datum-literals (:)
         [x:id #'x]
         [[x:id : t] #'x]))
     #'(define (name param ...)
         (λ (s)
           (stream-lazy
            ((conj g ...) s))))]))

(define-syntax run
  (syntax-rules ()
    [(_ n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== (list x0 x ...) q)
                g ...))]
    [(_ n q g ...)
     (let ([q (var 'q)])
       (map (reify q) (run-goal n (conj g ...))))]))

(define-syntax-rule (run* q g ...) (run #f q g ...))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x0 x ...) g ...)
     (call/fresh 'x0
                 (λ (x0)
                   (fresh (x ...) g ...)))]))

(define-syntax-rule (conde [g ...] ...)
  (disj (conj g ...) ...))

(define-syntax conda
  (syntax-rules ()
    [(_ [g0 g ...]) (conj g0 g ...)]
    [(_ [g0 g ...] ln ...)
     (ifte g0 (conj g ...) (conda ln ...))]))

(define-syntax-rule (condu [g0 g ...] ...)
  (conda [(once g0) g ...] ...))

(define-syntax-rule (project (x ...) g ...)
  (λ (s)
    (let ([x (walk* x s)] ...)
      ((conj g ...) s))))
