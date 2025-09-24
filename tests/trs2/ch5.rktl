(displayln "Test code in chapter 5")
(: appendo (→ Term Term Term Goal))
(define (appendo l s out)
  (conde
    [(nullo l) (== s out)]
    [else
     (fresh (a d res)
       (conso a d l)
       (conso a res out)
       (appendo d s res))]))
(check-equal?
 (run* (x)
   (appendo
    '(cake)
    '(tastes yummy)
    x))
 '((cake tastes yummy)))
(check-equal?
 (run* (x)
   (fresh (y)
     (appendo
      `(cake with ice ,y)
      '(tastes yummy)
      x)))
 '((cake with ice _.0 tastes yummy)))
(check-equal?
 (run* (x)
   (fresh (y)
     (appendo
      '(cake with ice cream)
      y
      x)))
 '((cake with ice cream . _.0)))
(check-equal?
 (run 1 (x)
   (fresh (y)
     (appendo `(cake with ice . ,y) '(d t) x)))
 '((cake with ice d t)))
(check-equal?
 (run 5 (x)
   (fresh (y)
     (appendo `(cake with ice . ,y) '(d t) x)))
 '((cake with ice d t)
   (cake with ice _.0 d t)
   (cake with ice _.0 _.1 d t)
   (cake with ice _.0 _.1 _.2 d t)
   (cake with ice _.0 _.1 _.2 _.3 d t)))
(check-equal?
 (run 5 (y)
   (fresh (x)
     (appendo `(cake with ice . ,y) '(d t) x)))
 '(()
   (_.0)
   (_.0 _.1)
   (_.0 _.1 _.2)
   (_.0 _.1 _.2 _.3)))
(check-equal?
 (run 5 (x)
   (fresh (y)
     (appendo
      `(cake with ice . ,y)
      `(d t . ,y)
      x)))
 '((cake with ice d t)
   (cake with ice _.0 d t _.0)
   (cake with ice _.0 _.1 d t _.0 _.1)
   (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
   (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3)))
(check-equal?
 (run* (x)
   (fresh (z)
     (appendo
      '(cake with ice cream)
      `(d t . ,z)
      x)))
 '((cake with ice cream d t . _.0)))
(check-equal?
 (run 6 (x)
   (fresh (y)
     (appendo x y '(cake with ice d t))))
 '(()
   (cake)
   (cake with)
   (cake with ice)
   (cake with ice d)
   (cake with ice d t)))
(check-equal?
 (run 6 (y)
   (fresh (x)
     (appendo x y '(cake with ice d t))))
 '((cake with ice d t)
   (with ice d t)
   (ice d t)
   (d t)
   (t)
   ()))
(check-equal?
 (run 6 (r)
   (fresh (x y)
     (appendo x y '(cake with ice d t))
     (== `(,x ,y) r)))
 '((() (cake with ice d t))
   ((cake) (with ice d t))
   ((cake with) (ice d t))
   ((cake with ice) (d t))
   ((cake with ice d) (t))
   ((cake with ice d t) ())))
(check-equal?
 (run 7 (r)
   (fresh (x y)
     (appendo x y '(cake with ice d t))
     (== `(,x ,y) r)))
 '((() (cake with ice d t))
   ((cake) (with ice d t))
   ((cake with) (ice d t))
   ((cake with ice) (d t))
   ((cake with ice d) (t))
   ((cake with ice d t) ())))
(check-equal?
 (run 7 (x)
   (fresh (y z)
     (appendo x y z)))
 '(()
   (_.0)
   (_.0 _.1)
   (_.0 _.1 _.2)
   (_.0 _.1 _.2 _.3)
   (_.0 _.1 _.2 _.3 _.4)
   (_.0 _.1 _.2 _.3 _.4 _.5)))
(check-equal?
 (run 7 (y)
   (fresh (x z)
     (appendo x y z)))
 '(_.0
   _.0
   _.0
   _.0
   _.0
   _.0
   _.0))
(check-equal?
 (run 7 (z)
   (fresh (x y)
     (appendo x y z)))
 '(_.0
   (_.0 . _.1)
   (_.0 _.1 . _.2)
   (_.0 _.1 _.2 . _.3)
   (_.0 _.1 _.2 _.3 . _.4)
   (_.0 _.1 _.2 _.3 _.4 . _.5)
   (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))
(check-equal?
 (run 7 (r)
   (fresh (x y z)
     (appendo x y z)
     (== `(,x ,y ,z) r)))
 '((() _.0 _.0)
   ((_.0) _.1 (_.0 . _.1))
   ((_.0 _.1) _.2 (_.0 _.1 . _.2))
   ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
   ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
   ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
   ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))

(: ll (→ Index Var Goal Goal))
(define ((ll n x g) s)
  (match (walk x s)
    [(? var?) (g (ext-s x 1 s))]
    [(? index? v) #:when (< v n) (g (ext-s x (add1 v) s))]
    [_ (fail s)]))
(define-syntax-rule (λ-limited n formals g)
  (let ([x (var 'x)])
    (λ formals (ll n x g))))

(: swappendo (→ Term Term Term Goal))
(define swappendo
  (λ-limited 5 (l s out)
    (conde
      [succeed
       (fresh (a d res)
         (conso a d l)
         (conso a res out)
         (swappendo d s res))]
      [(nullo l) (== s out)])))
(check-equal?
 (run* (z)
   (fresh (x y)
     (swappendo x y z)))
 '((_.0 _.1 _.2 _.3 . _.4)
   (_.0 _.1 _.2 . _.3)
   (_.0 _.1 . _.2)
   (_.0 . _.1)
   _.0))

(: unwrapo (→ Term Term Goal))
(define (unwrapo x out)
  (conde
    [succeed (== x out)]
    [else
     (fresh (a)
       (caro x a)
       (unwrapo a out))]))
(check-equal?
 (run* (x)
   (unwrapo '(((pizza))) x))
 '((((pizza)))
   ((pizza))
   (pizza)
   pizza))
(check-equal?
 (run 5 (x)
   (unwrapo x 'pizza))
 '(pizza
   (pizza . _.0)
   ((pizza . _.0) . _.1)
   (((pizza . _.0) . _.1) . _.2)
   ((((pizza . _.0) . _.1) . _.2) . _.3)))
(check-equal?
 (run 5 (x)
   (unwrapo x '((pizza))))
 '(((pizza))
   (((pizza)) . _.0)
   ((((pizza)) . _.0) . _.1)
   (((((pizza)) . _.0) . _.1) . _.2)
   ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))
(check-equal?
 (run 5 (x)
   (unwrapo x '((pizza))))
 '(((pizza))
   (((pizza)) . _.0)
   ((((pizza)) . _.0) . _.1)
   (((((pizza)) . _.0) . _.1) . _.2)
   ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))
(check-equal?
 (run 5 (x)
   (unwrapo `((,x)) 'pizza))
 '(pizza
   (pizza . _.0)
   ((pizza . _.0) . _.1)
   (((pizza . _.0) . _.1) . _.2)
   ((((pizza . _.0) . _.1) . _.2) . _.3)))

(: flatteno (→ Term Term Goal))
(define (flatteno s out)
  (conde
    [(nullo s) (== '() out)]
    [(pairo s)
     (fresh (a d res-a res-d)
       (conso a d s)
       (flatteno a res-a)
       (flatteno d res-d)
       (appendo res-a res-d out))]
    [else (conso s '() out)]))
(check-equal?
 (run 1 (x)
   (flatteno '((a b) c) x))
 '((a b c)))
(check-equal?
 (run 1 (x)
   (flatteno '(a (b c)) x))
 '((a b c)))
(check-equal?
 (run* (x)
   (flatteno '(a) x))
 '((a)
   (a ())
   ((a))))
(check-equal?
 (run* (x)
   (flatteno '((a)) x))
 '((a)
   (a ())
   (a ())
   (a () ())
   ((a))
   ((a) ())
   (((a)))))
(check-equal?
 (run* (x)
   (flatteno '(((a))) x))
 '((a)
   (a ())
   (a ())
   (a () ())
   (a ())
   (a () ())
   (a () ())
   (a () () ())
   ((a))
   ((a) ())
   ((a) ())
   ((a) () ())
   (((a)))
   (((a)) ())
   ((((a))))))
(check-equal?
 (run* (x)
   (flatteno '((a b) c) x))
 '((a b c)
   (a b c ())
   (a b (c))
   (a b () c)
   (a b () c ())
   (a b () (c))
   (a (b) c)
   (a (b) c ())
   (a (b) (c))
   ((a b) c)
   ((a b) c ())
   ((a b) (c))
   (((a b) c))))

(: flattenrevo (→ Term Term Goal))
(define (flattenrevo s out)
  (conde
    [succeed (conso s '() out)]
    [(nullo s) (== '() out)]
    [else
     (fresh (a d res-a res-d)
       (conso a d s)
       (flattenrevo a res-a)
       (flattenrevo d res-d)
       (appendo res-a res-d out))]))
(check-equal?
 (run* (x)
   (flattenrevo '((a b) c) x))
 (reverse
  (run* (x)
    (flatteno '((a b) c) x))))
(check-equal?
 (run 2 (x)
   (flattenrevo x '(a b c)))
 '((a b . c)
   (a b c)))
(check-equal?
 (length
  (run* (x)
    (flattenrevo '((((a (((b))) c))) d) x)))
 574)
