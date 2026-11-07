#lang racket/base

;; miniKanren as implemented in The Reasoned Schemer, first edition,
;; using private/racket/stream.
;;
;; Source: Daniel P. Friedman, William E. Byrd, and Oleg Kiselyov,
;; The Reasoned Schemer, MIT Press, 2005 -- chapter 9 ("Under the Hood")
;; and appendix A ("Connecting the Wires").  The same code is published,
;; without a license, at https://github.com/miniKanren/TheReasonedSchemer.
;; The original code is Copyright (C) 2005 Massachusetts Institute of
;; Technology.  See NOTICE for details and for the changes made here.


(require private/racket/stream)

(provide (all-defined-out))


(struct var (name)
  #:transparent)

(define (data? v)
  (or (boolean? v) (number? v) (char? v) (bytes? v) (string? v)
      (keyword? v) (null? v) (symbol? v)
      (and (pair? v) (data? (car v)) (data? (cdr v)))))

(define empty-s '())
(define (size-s s) (length s))
(define (ext-s x v s)
  (and (or (data? v) (not (occurs? x v s)))
       (cons (cons x v) s)))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (eq? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))

(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(assq v s)
        => (λ (a) (walk (cdr a) s))]
       [else v])]
    [else v]))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))

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

(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (ext-s v (reify-name (size-s s)) s)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify v) (walk* v (reify-s v empty-s)))


(define-syntax-rule (λF body ...)
  (stream-lazy (let () body ...)))
(define-syntax-rule (λG (x) body ...)
  (λ (x) body ...))

(define-syntax-rule (run n^ (x) g ...)
  (let ([n n^] [x (var 'x)])
    (if (or (not n) (> n 0))
        (map∞ n
              (λ (s) (reify (walk* x s)))
              ((all g ...) empty-s))
        '())))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax-rule
  (case∞ e
    [() on-zero]
    [(a^) on-one]
    [(a f) on-choice])
  (let ([a∞ (stream-force e)])
    (cond
      [(stream-empty? a∞)
       on-zero]
      [(eq? (stream-peek-rest a∞) empty-stream)
       (let ([a^ (car (stream-peek-first a∞))])
         on-one)]
      [else
       (let ([a (car (stream-peek-first a∞))] [f (stream-peek-rest a∞)])
         on-choice)])))

(define-syntax-rule (mzero) empty-stream)
(define-syntax-rule (unit a) (stream-cons #:eager a #:eager empty-stream))
(define-syntax-rule (choice a f) (stream-cons #:eager a #:eager f))

(define (map∞ n p a∞)
  (case∞ a∞
    [() '()]
    [(a) (list (p a))]
    [(a f)
     (cons
      (p a)
      (cond
        [(not n) (map∞ n p (stream-force-once f))]
        [(> n 1) (map∞ (sub1 n) p (stream-force-once f))]
        [else '()]))]))

(define succeed (λG (s) (unit s)))
(define fail (λG (s) (mzero)))

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


(define (mplus a∞ f)
  (case∞ a∞
    [() (stream-force-once f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF (mplus (stream-force-once f0) f)))]))

(define (bind a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplus (g a) (λF (bind (stream-force-once f) g)))]))

(define (mplusi a∞ f)
  (case∞ a∞
    [() (stream-force-once f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF (mplusi (stream-force-once f) f0)))]))

(define (bindi a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplusi (g a) (λF (bindi (stream-force-once f) g)))]))

(define-syntax-rule (ife g0 g1 g2)
  (λG (s) (mplus ((all g0 g1) s) (λF (g2 s)))))

(define-syntax-rule (ifi g0 g1 g2)
  (λG (s) (mplusi ((all g0 g1) s) (λF (g2 s)))))

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
