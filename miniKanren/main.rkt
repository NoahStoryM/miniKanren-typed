#lang typed/racket/base

(provide (all-defined-out))


(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Data (∪ Literal (Pair Data Data)))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))
(define-type (Streamof a) (∪ Null (Boxof a) (Pair a (→ (Streamof a)))))
(define-type Goal (→ Substitution (Streamof Substitution)))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)

(define-predicate data? Data)

(: empty-s Substitution)
(: size-s (→ Substitution Index))
(: ext-s (case→
          (→ Var Data Substitution Substitution)
          (→ Var Term Substitution (Option Substitution))))
(define empty-s '())
(define (size-s s) (length s))
(define (ext-s x v s)
  (and (or (data? v) (not (occurs? x v s)))
       (cons (cons x v) s)))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (eq? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))

(: walk (→ Term Substitution Term))
(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(assq v s)
        => (λ (a) (walk (cdr a) s))]
       [else v])]
    [else v]))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))

(: unify (→ Term Term Substitution (Option Substitution)))
(define (unify v w s)
  (let ([v (walk v s)] [w (walk w s)])
    (cond
      [(eq? v w) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (cond
         [(unify (car v) (car w) s)
          => (λ (s) (unify (cdr v) (cdr w) s))]
         [else #f])]
      [(equal? v w) s]
      [else #f])))

(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (ext-s v (reify-name (size-s s)) s)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(: reify (→ Term Term))
(define (reify v) (walk* v (reify-s v empty-s)))


(define-syntax λF
  (syntax-rules (:)
    [(_ : a body ...)
     (λ () : (Streamof a) body ...)]))
(define-syntax-rule (λG (x) body ...)
  (ann (λ (x) body ...) Goal))

(define-syntax-rule (run n^ (x) g ...)
  (let ([n n^] [x (var 'x)])
    (if (or (not n) (> n 0))
        (map∞ n
              (λ ([s : Substitution]) (reify (walk* x s)))
              ((all g ...) empty-s))
        '())))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax-rule
  (case∞ e
    [() on-zero]
    [(a^) on-one]
    [(a f) on-choice])
  (let ([a∞ e])
    (cond
      [(null? a∞)
       on-zero]
      [(box? a∞)
       (let ([a^ (unbox a∞)])
         on-one)]
      [else
       (let ([a (car a∞)] [f (cdr a∞)])
         on-choice)])))

(define-syntax-rule (mzero) '())
(define-syntax-rule (unit a) (box-immutable a))
(define-syntax-rule (choice a f) (cons a f))

(: map∞ (∀ (a b) (→ (Option Integer) (→ a b) (Streamof a) (Listof b))))
(define (map∞ n p a∞)
  (case∞ a∞
    [() '()]
    [(a) (list (p a))]
    [(a f)
     (cons
      (p a)
      (cond
        [(not n) (map∞ n p (f))]
        [(> n 1) (map∞ (sub1 n) p (f))]
        [else '()]))]))

(define succeed (λG (s) (unit s)))
(define fail (λG (s) (mzero)))

(: == (→ Term Term Goal))
(define (== v w)
  (λG (s)
    (cond
      [(unify v w s) => succeed]
      [else (fail s)])))

(define-syntax-rule (fresh (x ...) g ...)
  (λG (s)
    (let ([x (var 'x)] ...)
      ((all g ...) s))))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ ifer) fail]
    [(_ ifer [else g ...]) (all g ...)]
    [(_ ifer [g ...]) (all g ...)]
    [(_ ifer [g0 g ...] c ...)
     (ifer g0 (all g ...) (cond-aux ifer c ...))]))
(define-syntax-rule (conde c ...) (cond-aux ife c ...))
(define-syntax-rule (condi c ...) (cond-aux ifi c ...))
(define-syntax-rule (conda c ...) (cond-aux ifa c ...))
(define-syntax-rule (condu c ...) (cond-aux ifu c ...))

(define-syntax all-aux
  (syntax-rules ()
    [(_ bnd) succeed]
    [(_ bnd g) g]
    [(_ bnd g0 g ...)
     (let ([g^ g0])
       (λG (s)
         (bnd (g^ s)
              (all-aux bnd g ...))))]))
(define-syntax-rule (all  g ...) (all-aux bind  g ...))
(define-syntax-rule (alli g ...) (all-aux bindi g ...))


(: mplus (∀ (a) (→ (Streamof a) (→ (Streamof a)) (Streamof a))))
(define (mplus a∞ f)
  (case∞ a∞
    [() (f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF : a (mplus (f0) f)))]))

(: bind (∀ (a) (→ (Streamof a) (→ a (Streamof a)) (Streamof a))))
(define (bind a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplus (g a) (λF : a (bind (f) g)))]))

(: mplusi (∀ (a) (→ (Streamof a) (→ (Streamof a)) (Streamof a))))
(define (mplusi a∞ f)
  (case∞ a∞
    [() (f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF : a (mplusi (f) f0)))]))

(: bindi (∀ (a) (→ (Streamof a) (→ a (Streamof a)) (Streamof a))))
(define (bindi a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplusi (g a) (λF : a (bindi (f) g)))]))

(define-syntax-rule (ife g0 g1 g2)
  (λG (s) (mplus ((all g0 g1) s) (λF : Substitution (g2 s)))))

(define-syntax-rule (ifi g0 g1 g2)
  (λG (s) (mplusi ((all g0 g1) s) (λF : Substitution (g2 s)))))

(define-syntax-rule (ifa g0 g1 g2)
  (λG (s)
    (let ([s∞ (g0 s)])
      (case∞ s∞
        [() (g2 s)]
        [(s) (g1 s)]
        [(s f) (bind s∞ g1)]))))

(define-syntax-rule (ifu g0 g1 g2)
  (λG (s)
    (let ([s∞ (g0 s)])
      (case∞ s∞
        [() (g2 s)]
        [(s) (g1 s)]
        [(s f) (g1 s)]))))
