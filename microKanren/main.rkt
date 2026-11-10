#lang typed/racket/base

;; microKanren, rewritten in Typed Racket.
;;
;; Source: https://github.com/jasonhemann/microKanren -- microKanren.scm
;; (the core) and miniKanren-wrappers.scm (the user-level forms), from
;; Jason Hemann and Daniel P. Friedman, "μKanren: A Minimal Functional
;; Core for Relational Programming", Scheme Workshop 2013.
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.  MIT License;
;; see NOTICE for the full text and for the changes made here.

(provide (all-defined-out))


;;; Terms, substitutions, and states

(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))
;; A state is a substitution and the counter for the next fresh variable.
(define-type State (Pair Substitution Natural))

;; A variable is identified by its counter.
(struct var ([c : Natural])
  #:type-name Var
  #:transparent)

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-c x1) (var-c x2)))

(: walk (→ Term Substitution Term))
(define (walk u s)
  (let ([pr (and (var? u) (assf (λ ([v : Var]) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

(: ext-s (→ Var Term Substitution Substitution))
(define (ext-s x v s) (cons (cons x v) s))


;;; Streams

;; A stream is empty, or a state followed by a stream (mature), or a
;; suspension that produces a stream when called (immature).
(define-type (Streamof a) (∪ Null (Pair a (Streamof a)) (Suspension a)))
(define-type (Suspension a) (→ (Streamof a)))

(define-type Goal (→ State (Streamof State)))

;; Read element by element, a stream is a lazy list whose elements are
;; states or PAUSE: a suspension is a pause followed by the stream it
;; produces.  So (Streamof a) is a lazy list of (Maybe a), and the
;; operations below are the only code that knows how the three cases are
;; represented:
;;
;;   '()                   the empty stream
;;   (stream-cons a d)     the state a followed by the stream d
;;   (stream-cons pause d) a pause followed by d: the suspension
;;                         (λ () d), so d is evaluated only when forced
;;   (stream-lazy d)       the same as (stream-cons pause d)
;;   stream-empty?         whether a stream is empty
;;   stream-first          the first element of a nonempty stream: a
;;                         state, or pause for an immature stream
;;   stream-rest           the rest of a nonempty stream, forcing the
;;                         suspension of an immature one
;;
;; Maybe is a union, not a tagged sum, so the states themselves must not
;; be pauses.
(struct Pause () #:type-name Pause)
(define pause (Pause))
(define-predicate pause? Pause)
(define-type (Maybe a) (∪ Pause a))

(define stream-empty? null?)

(: stream-first (∀ (a) (→ (∪ (Pair a (Streamof a)) (Suspension a)) (Maybe a))))
(: stream-rest  (∀ (a) (→ (∪ (Pair a (Streamof a)) (Suspension a)) (Streamof a))))
(define (stream-first $) (if (pair? $) (car $) pause))
(define (stream-rest  $) (if (pair? $) (cdr $) ($)))

(define-syntax stream-cons
  (syntax-rules (pause)
    [(_ pause $) (λ () $)]
    [(_ a $) (cons a $)]))
(define-syntax-rule (stream-lazy $) (stream-cons pause $))

(: unit (∀ (a) (→ a (Streamof a))))
(define (unit s/c) (stream-cons s/c mzero))

(: mzero Null)
(define mzero '())

;; Swaps the two streams at each pause, which makes the search interleave.
(: mplus (∀ (a) (→ (Streamof a) (Streamof a) (Streamof a))))
(define (mplus $1 $2)
  (cond
    [(stream-empty? $1) $2]
    [else
     (let ([a (stream-first $1)])
       (if (pause? a)
           (stream-lazy (mplus $2 (stream-rest $1)))
           (stream-cons a (mplus (stream-rest $1) $2))))]))

(: bind (∀ (a b) (→ (Streamof a) (→ a (Streamof b)) (Streamof b))))
(define (bind $ g)
  (cond
    [(stream-empty? $) mzero]
    [else
     (let ([a (stream-first $)])
       (if (pause? a)
           (stream-lazy (bind (stream-rest $) g))
           (mplus (g a) (bind (stream-rest $) g))))]))


;;; Goals

(: == (→ Term Term Goal))
(define ((== u v) s/c)
  (let ([s (unify u v (car s/c))])
    (if s (unit (cons s (cdr s/c))) mzero)))

(: unify (→ Term Term Substitution (Option Substitution)))
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

(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (let ([c (cdr s/c)])
    ((f (var c)) (cons (car s/c) (+ c 1)))))

(: disj (→ Goal Goal Goal))
(define ((disj g1 g2) s/c) (mplus (g1 s/c) (g2 s/c)))

(: conj (→ Goal Goal Goal))
(define ((conj g1 g2) s/c) (bind (g1 s/c) g2))


;;; User-level forms (miniKanren-wrappers.scm)

;; Inverse-η-delay: suspends the goal, so that a recursive relation
;; produces an immature stream instead of looping.
(define-syntax-rule (Zzz g)
  (λ ([s/c : State]) (stream-lazy (g s/c))))

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
      (λ ([x0 : Var])
        (fresh (x ...) g0 g ...)))]))

(define-syntax-rule (conde [g0 g ...] ...)
  (disj+ (conj+ g0 g ...) ...))

(define-syntax-rule (run n (x ...) g0 g ...)
  (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))

(define-syntax-rule (run* (x ...) g0 g ...)
  (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))

(: empty-state State)
(define empty-state (cons '() 0))

(: call/goal (→ Goal (Streamof State)))
(define (call/goal g) (g empty-state))

;; Skips the pauses at the front of a stream.  The result is mature or
;; empty, which its type records as an ordinary list cell, so take and
;; take-all read it with car and cdr.
(: pull (∀ (a) (→ (Streamof a) (∪ Null (Pair a (Streamof a))))))
(define (pull $)
  (cond
    [(stream-empty? $) '()]
    [else
     (let ([a (stream-first $)])
       (if (pause? a)
           (pull (stream-rest $))
           (stream-cons a (stream-rest $))))]))

(: take-all (∀ (a) (→ (Streamof a) (Listof a))))
(define (take-all $)
  (let ([$ (pull $)])
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(: take (∀ (a) (→ Natural (Streamof a) (Listof a))))
(define (take n $)
  (if (zero? n)
      '()
      (let ([$ (pull $)])
        (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

;; The value of the first variable, with its fresh variables reified.
(: reify-1st (→ State Term))
(define (reify-1st s/c)
  (let ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (length s))])
         (cons (cons v n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(: reify-name (→ Natural Symbol))
(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

;; Applies f to n fresh variables.
(: fresh/nf (→ Natural (→ Var * Goal) Goal))
(define (fresh/nf n f)
  (let app-f/v* ([n : Natural n] [v* : (Listof Var) '()])
    (cond
      [(zero? n) (apply f (reverse v*))]
      [else (call/fresh
             (λ ([x : Var])
               (app-f/v* (- n 1) (cons x v*))))])))
