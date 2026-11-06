;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 3.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 3")

(defrel (listo l) (conde ((nullo l)) ((fresh (d) (cdro l d) (listo d)))))
(defrel (lolo l)
  (conde
   ((nullo l))
   ((fresh (a) (caro l a) (listo a)) (fresh (d) (cdro l d) (lolo d)))))
(defrel (loso l)
  (conde
   ((nullo l))
   ((fresh (a) (caro l a) (singletono a)) (fresh (d) (cdro l d) (loso d)))))
(defrel (membero x l)
  (conde ((caro l x)) ((fresh (d) (cdro l d) (membero x d)))))
(defrel (proper-membero x l)
  (conde
   ((caro l x) (fresh (d) (cdro l d) (listo d)))
   ((fresh (d) (cdro l d) (proper-membero x d)))))
(: lol? (-> (Listof Term) Boolean))
(define (lol? l) (cond ((null? l) #t) ((list? (car l)) (lol? (cdr l))) (#t #f)))
(: member? (-> Term (Listof Term) Boolean))
(define (member? x l)
  (cond ((null? l) #f) ((equal? (car l) x) #t) (#t (member? x (cdr l)))))
;; 1-4, 9, 12, 14, 18, 20, 23-27, 29, 34, 38, 41, 43-44.
(check-equal? (list? '((a) (a b) c)) '#t)
(check-equal? (list? '()) '#t)
(check-equal? (list? 's) '#f)
(check-equal? (list? '(d a t e . s)) '#f)
(check-equal? (run* x (listo `(a b ,x d))) '(_0))
;; 12: nonterminating: (run* x (listo `(a b c . ,x)))
(check-equal? (run 1 x (listo `(a b c . ,x))) '(()))
(check-equal?
 (run 5 x (listo `(a b c . ,x)))
 '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3)))
(check-equal?
 (run
  7
  (x y)
  (conde
   ((== 'split x) (== 'pea y))
   ((== 'red x) (== 'bean y))
   ((== 'green x) (== 'lentil y))))
 '((split pea) (red bean) (green lentil)))
(check-equal? (run* q (fresh (x y) (lolo `((a b) (,x c) (d ,y))))) '(_0))
(check-equal? (run 1 l (lolo l)) '(()))
(check-equal? (run 1 q (fresh (x) (lolo `((a b) . ,x)))) '(_0))
(check-equal? (run 1 x (lolo `((a b) (c d) . ,x))) '(()))
(check-equal?
 (run 5 x (lolo `((a b) (c d) . ,x)))
 '(() (()) ((_0)) (() ()) ((_0 _1))))
(check-equal? (run 5 x (lolo x)) '(() (()) ((_0)) (() ()) ((_0 _1))))
(check-equal? (run 1 z (loso `((g) . ,z))) '(()))
(check-equal?
 (run 5 z (loso `((g) . ,z)))
 '(() ((_0)) ((_0) (_1)) ((_0) (_1) (_2)) ((_0) (_1) (_2) (_3))))
(check-equal?
 (run
  4
  r
  (fresh (w x y z)
    (loso `((g) (e . ,w) (,x . ,y) . ,z))
    (== `(,w (,x . ,y) ,z) r)))
 '((() (_0) ())
   (() (_0) ((_1)))
   (() (_0) ((_1) (_2)))
   (() (_0) ((_1) (_2) (_3)))))
(check-equal?
 (run
  3
  out
  (fresh (w x y z) (== `((g) (e . ,w) (,x . ,y) . ,z) out) (loso out)))
 '(((g) (e) (_0)) ((g) (e) (_0) (_1)) ((g) (e) (_0) (_1) (_2))))
(check-equal? (member? 'olive '(virgin olive oil)) '#t)
;; 48-53, 55-56, 59-61, 63-64, 66-67, 74.
(check-equal? (run* q (membero 'olive '(virgin olive oil))) '(_0))
(check-equal? (run 1 y (membero y '(hummus with pita))) '(hummus))
(check-equal? (run 1 y (membero y '(with pita))) '(with))
(check-equal? (run 1 y (membero y '(pita))) '(pita))
(check-equal? (run* y (membero y '())) '())
(check-equal? (run* y (membero y '(hummus with pita))) '(hummus with pita))
(check-equal? (run* y (membero y '(pear grape . peaches))) '(pear grape))
(check-equal? (run* x (membero 'e `(pasta ,x fagioli))) '(e))
(check-equal? (run 1 x (membero 'e `(pasta e ,x fagioli))) '(_0))
(check-equal? (run 1 x (membero 'e `(pasta ,x e fagioli))) '(e))
(check-equal? (run* (x y) (membero 'e `(pasta ,x fagioli ,y))) '((e _0) (_0 e)))
(check-equal?
 (run* q (fresh (x y) (== `(pasta ,x fagioli ,y) q) (membero 'e q)))
 '((pasta e fagioli _0) (pasta _0 fagioli e)))
(check-equal? (run 1 l (membero 'tofu l)) '((tofu . _0)))
;; 66: nonterminating: (run* l (membero 'tofu l))
(check-equal?
 (run 5 l (membero 'tofu l))
 '((tofu . _0)
   (_0 tofu . _1)
   (_0 _1 tofu . _2)
   (_0 _1 _2 tofu . _3)
   (_0 _1 _2 _3 tofu . _4)))
(check-equal?
 (run 12 l (proper-membero 'tofu l))
 '((tofu)
   (tofu _0)
   (tofu _0 _1)
   (_0 tofu)
   (tofu _0 _1 _2)
   (tofu _0 _1 _2 _3)
   (_0 tofu _1)
   (tofu _0 _1 _2 _3 _4)
   (tofu _0 _1 _2 _3 _4 _5)
   (_0 tofu _1 _2)
   (tofu _0 _1 _2 _3 _4 _5 _6)
   (_0 _1 tofu)))
