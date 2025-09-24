(displayln "Test code in chapter 1")
(check-equal?
 (run* (q)
   fail)
 '())
(check-equal?
 (run* (q)
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   fail
   (== #t q))
 '())
(check-equal?
 (run* (q)
   succeed
   (== #t q))
 '(#t))

(check-equal?
 (run* (r)
   succeed
   (== 'corn r))
 '(corn))
(check-equal?
 (run* (r)
   fail
   (== 'corn r))
 '())

(check-equal?
 (run* (q)
   succeed
   (== #f q))
 '(#f))

(check-equal?
 (run* (x)
   (let ([x #f])
     (== #t x)))
 '())

(check-equal?
 (run* (q)
   (fresh (x)
     (== #t x)
     (== #t q)))
 '(#t))
(check-equal?
 (run* (q)
   (fresh (x)
     (== x #t)
     (== #t q)))
 '(#t))
(check-equal?
 (run* (q)
   (fresh (x)
     (== x #t)
     (== q #t)))
 '(#t))

(check-equal?
 (run* (x)
   succeed)
 '(_.0))
(check-equal?
 (run* (x)
   (let ([x #f])
     (fresh (x)
       (== #t x))))
 '(_.0))

(check-equal?
 (run* (r)
   (fresh (x y)
     (== (cons x (cons y '())) r)))
 '((_.0 _.1)))
(check-equal?
 (run* (s)
   (fresh (t u)
     (== (cons t (cons u '())) s)))
 '((_.0 _.1)))
(check-equal?
 (run* (r)
   (fresh (x)
     (let ([y x])
       (fresh (x)
         (== (cons y (cons x (cons y '()))) r)))))
 '((_.0 _.1 _.0)))
(check-equal?
 (run* (r)
   (fresh (x)
     (let ([y x])
       (fresh (x)
         (== (cons x (cons y (cons x '()))) r)))))
 '((_.0 _.1 _.0)))

(check-equal?
 (run* (q)
   (== #f q)
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (== #f q)
   (== #f q))
 '(#f))
(check-equal?
 (run* (q)
   (let ([x q])
     (== #t x)))
 '(#t))
(check-equal?
 (run* (q)
   (fresh (x)
     (== #t x)
     (== x q)))
 '(#t))
(check-equal?
 (run* (q)
   (fresh (x)
     (== x q)
     (== #t x)))
 '(#t))

(check-equal?
 (run* (x)
   (conde
     [(== 'olive x) succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '(olive oil))
(check-equal?
 (run 1 (x)
   (conde
     [(== 'olive x) succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '(olive))
(check-equal?
 (run* (x)
   (conde
     [(== 'virgin x) fail]
     [(== 'olive x) succeed]
     [succeed succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '(olive _.0 oil))
(check-equal?
 (run 2 (x)
   (conde
     [(== 'extra x) succeed]
     [(== 'virgin x) fail]
     [(== 'olive x) succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '(extra olive))
(check-equal?
 (run* (r)
   (fresh (x y)
     (== 'split x)
     (== 'pea y)
     (== (cons x (cons y '())) r)))
 '((split pea)))
(check-equal?
 (run* (r)
   (fresh (x y)
      (conde
        [(== 'split x) (== 'pea y)]
        [(== 'navy x) (== 'bean y)]
        [else fail])
      (== (cons x (cons y '())) r)))
 '((split pea) (navy bean)))
(check-equal?
 (run* (r)
   (fresh (x y)
     (conde
       [(== 'split x) (== 'pea y)]
       [(== 'navy x) (== 'bean y)]
       [else fail])
     (== (cons x (cons y (cons 'soup '()))) r)))
 '((split pea soup) (navy bean soup)))

(: teacupo (→ Term Goal))
(define (teacupo x)
  (conde
    [(== 'tea x) succeed]
    [(== 'cup x) succeed]
    [else fail]))
(check-equal?
 (run* (x)
   (teacupo x))
 '(tea cup))
(check-equal?
 (run* (r)
   (fresh (x y)
     (conde
       [(teacupo x) (== #t y) succeed]
       [(== #f x) (== #t y)]
       [else fail])
     (== (cons x (cons y '())) r)))
 '((tea #t) (cup #t) (#f #t)))
(check-equal?
 (run* (r)
   (fresh (x y z)
     (conde
       [(== y x) (fresh (x) (== z x))]
       [(fresh (x) (== y x)) (== z x)]
       [else fail])
     (== (cons y (cons z '())) r)))
 '((_.0 _.1) (_.0 _.1)))
(check-equal?
 (run* (r)
   (fresh (x y z)
     (conde
       [(== y x) (fresh (x) (== z x))]
       [(fresh (x) (== y x)) (== z x)]
       [else fail])
     (== #f x)
     (== (cons y (cons z '())) r)))
 '((#f _.0) (_.0 #f)))

(check-equal?
 (run* (q)
   (let ([a (== #t q)]
         [b (== #f q)])
     b))
 '(#f))
(check-equal?
 (run* (q)
   (let ([a (== #t q)]
         [b (fresh (x)
              (== x q)
              (== #f x))]
         [c (conde
              [(== #t q) succeed]
              [else (== #f q)])])
     b))
 '(#f))
