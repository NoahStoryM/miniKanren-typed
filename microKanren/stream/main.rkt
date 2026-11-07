#lang racket/base

;; microKanren, using private/racket/stream.
;;
;; Source: https://github.com/jasonhemann/microKanren -- microKanren.scm
;; (the core) and miniKanren-wrappers.scm (the user-level forms), from
;; Jason Hemann and Daniel P. Friedman, "μKanren: A Minimal Functional
;; Core for Relational Programming", Scheme Workshop 2013.
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.  MIT License;
;; see NOTICE for the full text and for the changes made here.

(require private/racket/stream)

(provide (all-defined-out))


;;; Terms, substitutions, and states

;; A variable is identified by its counter.
(struct var (c)
  #:transparent)

(define (var=? x1 x2) (= (var-c x1) (var-c x2)))

(define (walk u s)
  (let ([pr (and (var? u) (assf (λ (v) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) (cons (cons x v) s))


;;; Streams

;; Answers and tails are eager; only stream-lazy introduces a suspension.
(define (unit s/c) (stream-cons #:eager s/c #:eager mzero))

(define mzero empty-stream)

(define (mplus $1 $2)
  (cond
    [(eq? (stream-peek $1) empty-stream) $2]
    [(stream-peek-first $1)
     (stream-cons #:eager (car (stream-peek-first $1))
                  #:eager (mplus (stream-peek-rest $1) $2))]
    [(stream-peek $1) => (λ ($1) (mplus $1 $2))]
    [else (stream-lazy (mplus $2 (stream-force-once $1)))]))

(define (bind $ g)
  (cond
    [(eq? (stream-peek $) empty-stream) mzero]
    [(stream-peek-first $)
     (mplus (g (car (stream-peek-first $)))
            (bind (stream-peek-rest $) g))]
    [(stream-peek $) => (λ ($) (bind $ g))]
    [else (stream-lazy (bind (stream-force-once $) g))]))


;;; Goals

(define ((== u v) s/c)
  (let ([s (unify u v (car s/c))])
    (if s (unit (cons s (cdr s/c))) mzero)))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (equal? u v) s)])))

(define ((call/fresh f) s/c)
  (let ([c (cdr s/c)])
    ((f (var c)) (cons (car s/c) (+ c 1)))))

(define ((disj g1 g2) s/c) (mplus (g1 s/c) (g2 s/c)))

(define ((conj g1 g2) s/c) (bind (g1 s/c) g2))


;;; User-level forms (miniKanren-wrappers.scm)

;; Inverse-η-delay: suspends the goal, so that a recursive relation
;; produces an immature stream instead of looping.
(define-syntax-rule (Zzz g)
  (λ (s/c) (stream-lazy (g s/c))))

(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (disj (Zzz g0) (disj+ g ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g0 g ...) (conj+ g0 g ...)]
    [(_ (x0 x ...) g0 g ...)
     (call/fresh
      (λ (x0)
        (fresh (x ...) g0 g ...)))]))

(define-syntax-rule (conde [g0 g ...] ...)
  (disj+ (conj+ g0 g ...) ...))

(define-syntax-rule (run n (x ...) g0 g ...)
  (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))

(define-syntax-rule (run* (x ...) g0 g ...)
  (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))

;; A state is a substitution and the counter for the next fresh variable.
(define empty-state (cons '() 0))

(define (call/goal g) (g empty-state))

;; Forces a stream until it is mature or empty.
(define (pull $)
  (stream-force $))

(define (take-all $)
  (let ([$ (pull $)])
    (if (stream-empty? $)
        '()
        (cons (car (stream-peek-first $))
              (take-all (stream-peek-rest $))))))

(define (take n $)
  (if (zero? n)
      '()
      (let ([$ (pull $)])
        (if (stream-empty? $)
            '()
            (cons (car (stream-peek-first $))
                  (take (- n 1) (stream-peek-rest $)))))))

;; The value of the first variable, with its fresh variables reified.
(define (reify-1st s/c)
  (let ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (length s))])
         (cons (cons v n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

;; Applies f to n fresh variables.
(define (fresh/nf n f)
  (let app-f/v* ([n n] [v* '()])
    (cond
      [(zero? n) (apply f (reverse v*))]
      [else (call/fresh
             (λ (x)
               (app-f/v* (- n 1) (cons x v*))))])))
