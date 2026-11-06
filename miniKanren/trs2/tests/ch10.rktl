;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 10.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 10")

;; 5: use this implementation's public variable representation.
(define u (var 'u))

(define v (var 'v))

(define w (var 'w))

(define x (var 'x))

(define y (var 'y))

(define z (var 'z))
;; 7-8, 13-17, 24-28.
(check-equal?
 (cdr `(,z . b))
 'b)
(check-equal?
 (cdr `(,z ,x e ,y))
 `(,x e ,y))
(check-equal?
 (walk z `((,z . a) (,x . ,w) (,y . ,z)))
 'a)
(check-equal?
 (walk y `((,z . a) (,x . ,w) (,y . ,z)))
 'a)
(check-equal?
 (walk x `((,z . a) (,x . ,w) (,y . ,z)))
 w)
(check-equal?
 (walk x `((,x . ,y) (,v . ,x) (,w . ,x)))
 y)
(check-equal?
 (walk v `((,x . ,y) (,v . ,x) (,w . ,x)))
 y)
(check-equal?
 (walk w `((,x . ,y) (,v . ,x) (,w . ,x)))
 y)
(check-equal?
 (walk w `((,x . b) (,z . ,y) (,w ,x e ,z)))
 `(,x e ,z))
(check-equal?
 (occurs? x x '())
 '#t)
(check-equal?
 (occurs? x `(,y) `((,y . ,x)))
 '#t)
(check-equal?
 (ext-s x `(,x) empty-s)
 '#f)
(check-equal?
 (ext-s x `(,y) `((,y . ,x)))
 '#f)
(check-equal?
 (let ([s `((,z . ,x) (,y . ,z))])
   (let ([s (ext-s x 'e s)])
     (and s (walk y s))))
 'e)
;; 48-51, 53.
(check-equal?
 ((== #t #f) empty-s)
 '())
(check-equal?
 (fail empty-s)
 '())
(check-equal?
 ((== #f #f) empty-s)
 '(()))
(check-equal?
 (succeed empty-s)
 '(()))
(check-equal?
 ((== x y) empty-s)
 `(((,x . ,y))))
(check-equal?
 ((== y x) empty-s)
 `(((,y . ,x))))
(check-equal?
 ((disj2 (== 'olive x) (== 'oil x)) empty-s)
 `(((,x . olive)) ((,x . oil))))
;; 62-72: the answers describe stream shape, rather than concrete lists.
(check-equal?
 (procedure? ((nevero) empty-s))
 '#t)
(check-equal?
 (let ([s-inf ((disj2 (== 'olive x) (nevero)) empty-s)])
   (and (pair? s-inf) (car s-inf)))
 `((,x . olive)))
(check-equal?
 (let ([s-inf ((disj2 (nevero) (== 'olive x)) empty-s)])
   (procedure? s-inf))
 '#t)
(check-equal?
 (let ([s-inf ((disj2 (nevero) (== 'olive x)) empty-s)])
   (let ([s-inf ((assert s-inf procedure?))])
     (and (pair? s-inf) (car s-inf))))
 `((,x . olive)))
(check-equal?
 (let ([s-inf ((assert ((alwayso) empty-s) procedure?))])
   (and (pair? s-inf) (car s-inf)))
 '())
(check-equal?
 (let ([s-inf ((assert ((alwayso) empty-s) procedure?))])
   (cons (car (assert s-inf pair?)) '()))
 '(()))
(check-equal?
 (let ([s-inf ((assert ((alwayso) empty-s) procedure?))])
   (cons
    (car (assert s-inf pair?))
    (let ([s-inf ((assert (cdr (assert s-inf pair?)) procedure?))])
      (cons (car (assert s-inf pair?)) '()))))
 '(() ()))
(check-equal?
 (let ([s-inf ((assert ((alwayso) empty-s) procedure?))])
   (cons
    (car (assert s-inf pair?))
    (let ([s-inf ((assert (cdr (assert s-inf pair?)) procedure?))])
      (cons
       (car (assert s-inf pair?))
       (let ([s-inf ((assert (cdr (assert s-inf pair?)) procedure?))])
         (cons (car (assert s-inf pair?)) '()))))))
 '(() () ()))
;; 75: nonterminating: (take-inf 1 ((nevero) empty-s))
;; 77-80.
(check-equal?
 (take-inf 3 ((alwayso) empty-s))
 '(() () ()))
;; 78: nonterminating: (take-inf #f ((alwayso) empty-s))
(check-equal?
 (let ([k (length (take-inf 5 ((disj2 (== 'olive x) (== 'oil x)) empty-s)))])
   `(Found ,k not 5 substitutions))
 '(Found 2 not 5 substitutions))
(check-equal?
 (map (inst length (Pair Var Term))
      (take-inf 5 ((disj2 (== 'olive x) (== 'oil x)) empty-s)))
 '(1 1))
;; 91: the printed answer contains the new variable created by call/fresh.
;; Reify that variable to observe the association independently of its identity.
(check-equal?
 (map
  (lambda ([s : Substitution])
    (map (inst cdr Var Term) s))
  (take-inf
   1
   ((call/fresh
     'kiwi
     (lambda (fruit)
       (== 'plum fruit)))
    empty-s)))
 '((plum)))
;; 95-97, 113-117.
(check-equal?
 (reify-s `(,x ,y ,x ,z ,z) empty-s)
 `((,z . _2) (,y . _1) (,x . _0)))
(check-equal?
 (walk* `(,x ,y ,x ,z ,z) `((,z . _2) (,y . _1) (,x . _0)))
 '(_0 _1 _0 _2 _2))
(check-equal?
 (walk* w `((,x . b) (,z . ,y) (,w ,x e ,z)))
 `(b e ,y))
(check-equal?
 (let ([a1 `(,x ,u ,w ,y ,z ((ice) ,z))]
       [a2 `(,y . corn)]
       [a3 `(,w ,v ,u)])
   (let ([s `(,a1 ,a2 ,a3)])
     ((reify x) s)))
 '(_0 (_1 _0) corn _2 ((ice) _2)))
(check-equal?
 (map (reify x) (take-inf 5 ((disj2 (== 'olive x) (== 'oil x)) empty-s)))
 '(olive oil))
(check-equal?
 (map (reify x) (run-goal 5 (disj2 (== 'olive x) (== 'oil x))))
 '(olive oil))
;; 116-117: explicit low-level expansion, scoped independently of chapter 4.
(let ()
  (: appendo (→ Term Term Term Goal))
  (define (appendo l t out)
    (lambda (s)
      (lambda ()
        ((disj2
          (conj2 (nullo l) (== t out))
          (call/fresh
           'a
           (lambda (a)
             (call/fresh
              'd
              (lambda (d)
                (call/fresh
                 'res
                 (lambda (res)
                   (conj2 (conso a d l)
                          (conj2 (conso a res out) (appendo d t res))))))))))
         s))))
  (check-equal?
   (let ([q (var 'q)])
     (map
      (reify q)
      (run-goal
       #f
       (call/fresh
        'x
        (lambda (x)
          (call/fresh
           'y
           (lambda (y)
             (conj2 (== `(,x ,y) q) (appendo x y '(cake & ice d t))))))))))
   '((() (cake & ice d t))
     ((cake) (& ice d t))
     ((cake &) (ice d t))
     ((cake & ice) (d t))
     ((cake & ice d) (t))
     ((cake & ice d t) ()))))
;; 121-124, 125 footnote, 130.
(check-equal?
 ((ifte succeed (== #f y) (== #t y)) empty-s)
 `(((,y . #f))))
(check-equal?
 ((ifte fail (== #f y) (== #t y)) empty-s)
 `(((,y . #t))))
(check-equal?
 ((ifte (== #t x) (== #f y) (== #t y)) empty-s)
 `(((,y . #f) (,x . #t))))
(check-equal?
 ((ifte (disj2 (== #t x) (== #f x)) (== #f y) (== #t y)) empty-s)
 `(((,y . #f) (,x . #t)) ((,y . #f) (,x . #f))))
(check-equal?
 ((conda
    [(disj2 (== #t x) (== #f x)) (== #f y)]
    [(== #t y)])
  empty-s)
 `(((,y . #f) (,x . #t)) ((,y . #f) (,x . #f))))
(check-equal?
 ((ifte (once (disj2 (== #t x) (== #f x))) (== #f y) (== #t y)) empty-s)
 `(((,y . #f) (,x . #t))))
