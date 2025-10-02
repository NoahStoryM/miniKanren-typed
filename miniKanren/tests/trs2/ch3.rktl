(displayln "Test code in chapter 3")
(: listo (→ Term Goal))
(define (listo l)
  (conde
    [(nullo l) succeed]
    [(pairo l)
     (fresh (d)
       (cdro l d)
       (listo d))]
   [else fail]))
(check-equal?
 (run* (x)
   (listo `(a b ,x d)))
 '(_.0))
(check-equal?
 (run 1 (x)
   (listo `(a b c . ,x)))
 '(()))
(check-equal?
 (run 5 (x)
   (listo `(a b c . ,x)))
 '(()
   (_.0)
   (_.0 _.1)
   (_.0 _.1 _.2)
   (_.0 _.1 _.2 _.3)))

(: listofo (→ (→ Term Goal) Term Goal))
(define (listofo predo l)
  (conde
    [(nullo l) succeed]
    [(pairo l)
     (fresh (a)
       (caro l a)
       (predo a))
     (fresh (d)
       (cdro l d)
       (listofo predo d))]
    [else fail]))

(: lolo (→ Term Goal))
(define (lolo l) (listofo listo l))
(check-equal?
 (run 1 (l)
   (lolo l))
 '(()))
(check-equal?
 (run* (q)
   (fresh (x y)
     (lolo `((a b) (,x c) (d ,y)))
     (== #t q)))
 '(#t))
(check-equal?
 (run 1 (x)
   (lolo `((a b) (c d) . ,x)))
 '(()))
(check-equal?
 (run 5 (x)
   (lolo `((a b) (c d) . ,x)))
 '(()
   (())
   (() ())
   (() () ())
   (() () () ())))

(: twinso (→ Term Goal))
(define (twinso s)
  (fresh (x)
    (== `(,x ,x) s)))
(check-equal?
 (run* (q)
   (twinso '(tofu tofu))
   (== #t q))
 '(#t))
(check-equal?
 (run* (z)
   (twinso `(,z tofu)))
 '(tofu))

(: loto (→ Term Goal))
(define (loto l) (listofo twinso l))
(check-equal?
 (run 1 (z)
   (loto `((g g) . ,z)))
 '(()))
(check-equal?
 (run 5 (z)
   (loto `((g g) . ,z)))
 '(()
   ((_.0 _.0))
   ((_.0 _.0) (_.1 _.1))
   ((_.0 _.0) (_.1 _.1) (_.2 _.2))
   ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))))
(check-equal?
 (run 5 (r)
   (fresh (w x y z)
     (loto `((g g) (e ,w) (,x ,y) . ,z))
     (== `(,w (,x ,y) ,z) r)))
 '((e (_.0 _.0) ())
   (e (_.0 _.0) ((_.1 _.1)))
   (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
   (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
   (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))))
(check-equal?
 (run 3 (out)
   (fresh (w x y z)
     (== `((g g) (e ,w) (,x ,y) . ,z) out)
     (loto out)))
 '(((g g) (e e) (_.0 _.0))
   ((g g) (e e) (_.0 _.0) (_.1 _.1))
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

(check-equal?
 (run 3 (out)
   (fresh (w x y z)
     (== `((g g) (e ,w) (,x ,y) . ,z) out)
     (listofo twinso out)))
 '(((g g) (e e) (_.0 _.0))
   ((g g) (e e) (_.0 _.0) (_.1 _.1))
   ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

(: eq-car? (→ (Pair Term Term) Term Boolean))
(define (eq-car? l x) (eq? (car l) x))

(: eq-caro (→ Term Term Goal))
(define (eq-caro l x) (caro l x))

(: membero (→ Term Term Goal))
(define (membero x l)
  (conde
    [(nullo l) fail]
    [(eq-caro l x) succeed]
    [else
     (fresh (d)
       (cdro l d)
       (membero x d))]))
(check-equal?
 (run* (q)
   (membero 'olive '(virgin olive oil))
   (== #t q))
 '(#t))
(check-equal?
 (run 1 (y)
   (membero y '(hummus with pita)))
 '(hummus))
(check-equal?
 (run 1 (y)
   (membero y '(with pita)))
 '(with))
(check-equal?
 (run 1 (y)
   (membero y '(pita)))
 '(pita))
(check-equal?
 (run 1 (y)
   (membero y '()))
 '())
(check-equal?
 (run* (y)
   (membero y '(hummus with pita)))
 '(hummus with pita))

(: identity (→ (Listof Term) (Listof Term)))
(define (identity l)
  (run* (y)
    (membero y l)))
(check-equal?
 (run* (x)
   (membero 'e `(pasta ,x fagioli)))
 '(e))
(check-equal?
 (run 1 (x)
   (membero 'e `(e ,x fagioli)))
 '(_.0))
(check-equal?
 (run 1 (x)
   (membero 'e `(,x e fagioli)))
 '(e))
(check-equal?
 (run* (r)
   (fresh (x y)
     (membero 'e `(pasta ,x fagioli ,y))
     (== `(,x ,y) r)))
 '((e _.0) (_.0 e)))
(check-equal?
 (run 1 (l)
   (membero 'tofu l))
 '((tofu . _.0)))
(check-equal?
 (run 5 (l)
   (membero 'tofu l))
 '((tofu . _.0)
   (_.0 tofu . _.1)
   (_.0 _.1 tofu . _.2)
   (_.0 _.1 _.2 tofu . _.3)
   (_.0 _.1 _.2 _.3 tofu . _.4)))

(: pmembero1 (→ Term Term Goal))
(define (pmembero1 x l)
  (conde
    [(nullo l) fail]
    [(eq-caro l x) (cdro l '())]
    [else
     (fresh (d)
       (cdro l d)
       (pmembero1 x d))]))
(check-equal?
 (run 5 (l)
   (pmembero1 'tofu l))
 '((tofu)
   (_.0 tofu)
   (_.0 _.1 tofu)
   (_.0 _.1 _.2 tofu)
   (_.0 _.1 _.2 _.3 tofu)))

(: pmembero2 (→ Term Term Goal))
(define (pmembero2 x l)
  (conde
    [(nullo l) fail]
    [(eq-caro l x) (cdro l '())]
    [(eq-caro l x) succeed]
    [else
     (fresh (d)
       (cdro l d)
       (pmembero2 x d))]))
(check-equal?
 (run* (q)
   (pmembero2 'tofu '(a b tofu d tofu))
   (== #t q))
 '(#t #t #t))

(: pmembero3 (→ Term Term Goal))
(define (pmembero3 x l)
  (conde
    [(nullo l) fail]
    [(eq-caro l x) (cdro l '())]
    [(eq-caro l x)
     (fresh (a d)
       (cdro l `(,a . ,d)))]
    [else
     (fresh (d)
       (cdro l d)
       (pmembero3 x d))]))
(check-equal?
 (run* (q)
   (pmembero3 'tofu '(a b tofu d tofu))
   (== #t q))
 '(#t #t))
(check-equal?
 (run 12 (l)
   (pmembero3 'tofu l))
 '((tofu)
   (tofu _.0 . _.1)
   (_.0 tofu)
   (_.0 tofu _.1 . _.2)
   (_.0 _.1 tofu)
   (_.0 _.1 tofu _.2 . _.3)
   (_.0 _.1 _.2 tofu)
   (_.0 _.1 _.2 tofu _.3 . _.4)
   (_.0 _.1 _.2 _.3 tofu)
   (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
   (_.0 _.1 _.2 _.3 _.4 tofu)
   (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)))

(: pmembero4 (→ Term Term Goal))
(define (pmembero4 x l)
  (conde
    [(nullo l) fail]
    [(eq-caro l x)
     (fresh (a d)
       (cdro l `(,a . ,d)))]
    [(eq-caro l x) (cdro l '())]
    [else
     (fresh (d)
       (cdro l d)
       (pmembero4 x d))]))
(check-equal?
 (run 12 (l)
   (pmembero4 'tofu l))
 '((tofu _.0 . _.1)
   (tofu)
   (_.0 tofu _.1 . _.2)
   (_.0 tofu)
   (_.0 _.1 tofu _.2 . _.3)
   (_.0 _.1 tofu)
   (_.0 _.1 _.2 tofu _.3 . _.4)
   (_.0 _.1 _.2 tofu)
   (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
   (_.0 _.1 _.2 _.3 tofu)
   (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)
   (_.0 _.1 _.2 _.3 _.4 tofu)))

(: first-value (→ Term Term))
(define (first-value l) (run 1 (y) (membero y l)))
(check-equal? (first-value '(pasta e fagioli)) '(pasta))

(: memberrevo (→ Term Term Goal))
(define (memberrevo x l)
  (conde
    [(nullo l) fail]
    [succeed
     (fresh (d)
       (cdro l d)
       (memberrevo x d))]
    [else (eq-caro l x)]))
(check-equal?
 (run* (x)
   (memberrevo x '(pasta e fagioli)))
 '(fagioli e pasta))

(: reverse-list (→ (Listof Term) (Listof Term)))
(define (reverse-list l) (run* (y) (memberrevo y l)))
(check-equal?
 (reverse-list '(pasta e fagioli))
 (reverse '(pasta e fagioli)))
