#lang typed/racket/base

;; miniKanren as implemented in The Reasoned Schemer, second edition,
;; rewritten in Typed Racket.
;;
;; Source: https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd
;; (trs2-impl.scm), the code of chapter 10 ("Under the Hood") and
;; appendix A ("Connecting the Wires") of Daniel P. Friedman, William E.
;; Byrd, Oleg Kiselyov, and Jason Hemann, The Reasoned Schemer, second
;; edition, MIT Press, 2018.
;; Copyright (c) 2018 Daniel P. Friedman, William E. Byrd, Oleg Kiselyov,
;; and Jason Hemann.  MIT License; see NOTICE for the full text and for
;; the changes made here.

(require (for-syntax racket/base syntax/parse))

(provide (all-defined-out))


;;; Terms and substitutions

(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Data (∪ Literal (Pair Data Data)))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)

(define-predicate data? Data)

(: empty-s Substitution)
(define empty-s '())

(: walk (→ Term Substitution Term))
(define (walk v s)
  (cond
    [(and (var? v) (assq v s))
     => (λ (a) (walk (cdr a) s))]
    [else v]))

;; Extending with a ground term can never create a cycle, so that case
;; always succeeds; the type records this.
(: ext-s (case→
          (→ Var Data Substitution Substitution)
          (→ Var Term Substitution (Option Substitution))))
(define (ext-s x v s)
  (cond
    [(data? v) (cons (cons x v) s)]
    [(occurs? x v s) #f]
    [else (cons (cons x v) s)]))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(: unify (→ Term Term Substitution (Option Substitution)))
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

;; A stream is empty, or an answer followed by a stream (a "mature"
;; stream), or a suspension that produces a stream when forced (an
;; "immature" stream).  The three cases are told apart by null?, pair?,
;; and procedure?, none of which looks at the answers themselves, so no
;; extra tagging is needed.
(define-type (Streamof a) (∪ Null (Pair a (Streamof a)) (Suspension a)))
(define-type (Suspension a) (→ (Streamof a)))

(define-type Goal (→ Substitution (Streamof Substitution)))

;; mplus: interleaves by swapping the two streams at each suspension.
(: append-inf (∀ (a) (→ (Streamof a) (Streamof a) (Streamof a))))
(define (append-inf s-inf t-inf)
  (cond
    [(null? s-inf) t-inf]
    [(pair? s-inf)
     (cons (car s-inf)
           (append-inf (cdr s-inf) t-inf))]
    [else (λ () (append-inf t-inf (s-inf)))]))

;; bind, with the arguments in the order of append-map.
(: append-map-inf (∀ (a b) (→ (→ a (Streamof b)) (Streamof a) (Streamof b))))
(define (append-map-inf g s-inf)
  (cond
    [(null? s-inf) '()]
    [(pair? s-inf)
     (append-inf (g (car s-inf))
                 (append-map-inf g (cdr s-inf)))]
    [else (λ () (append-map-inf g (s-inf)))]))

(: take-inf (∀ (a) (→ (Option Natural) (Streamof a) (Listof a))))
(define (take-inf n s-inf)
  (cond
    [(and n (zero? n)) '()]
    [(null? s-inf) '()]
    [(pair? s-inf)
     (cons (car s-inf)
           (take-inf (and n (sub1 n)) (cdr s-inf)))]
    [else (take-inf n (s-inf))]))


;;; Goals

(: == (→ Term Term Goal))
(define ((== u v) s)
  (let ([s (unify u v s)])
    (if s (list s) '())))

(: succeed Goal)
(define (succeed s) (list s))

(: fail Goal)
(define (fail s) '())

(: disj2 (→ Goal Goal Goal))
(define ((disj2 g1 g2) s)
  (append-inf (g1 s) (g2 s)))

(: conj2 (→ Goal Goal Goal))
(define ((conj2 g1 g2) s)
  (append-map-inf g2 (g1 s)))

(: call/fresh (→ Symbol (→ Var Goal) Goal))
(define (call/fresh name f)
  (f (var name)))

(: ifte (→ Goal Goal Goal Goal))
(define ((ifte g1 g2 g3) s)
  (let loop ([s-inf (g1 s)])
    (cond
      [(null? s-inf) (g3 s)]
      [(pair? s-inf) (append-map-inf g2 s-inf)]
      [else (λ () (loop (s-inf)))])))

(: once (→ Goal Goal))
(define ((once g) s)
  (let loop ([s-inf (g s)])
    (cond
      [(null? s-inf) '()]
      [(pair? s-inf) (list (car s-inf))]
      [else (λ () (loop (s-inf)))])))


;;; Reification

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s))]
      [else v])))

(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_~a" n)))

;; The names are symbols, hence ground, so this ext-s cannot fail.
(: reify-s (→ Term Substitution Substitution))
(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v) (ext-s v (reify-name (length r)) r)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
      [else r])))

(: reify (→ Term (→ Substitution Term)))
(define ((reify v) s)
  (let* ([v (walk* v s)]
         [r (reify-s v empty-s)])
    (walk* v r)))

(: run-goal (→ (Option Natural) Goal (Listof Substitution)))
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

;; Every relation returns a Goal, and an argument is a Term unless it is
;; written [x : T], so defrel can supply the types itself.  The body is suspended:
;; this is where streams become immature.
(define-syntax (defrel stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (name:id arg ...) g ...)
     #:with (param ...)
     (for/list ([arg (in-list (syntax->list #'(arg ...)))])
       (syntax-parse arg
         #:datum-literals (:)
         [x:id #'[x : Term]]
         [[x:id : t] arg]))
     #'(define (name param ...) : Goal
         (λ (s)
           (λ ()
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
                 (λ ([x0 : Var])
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
  (λ ([s : Substitution])
    (let ([x (walk* x s)] ...)
      ((conj g ...) s))))
