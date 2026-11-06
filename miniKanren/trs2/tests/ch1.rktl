;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 1.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 1")

;; 7, 10-12, 14, 17, 19-21, 24-26, 31-38, 41-43.
(check-equal?
 (run* q
   fail)
 '())
(check-equal?
 (run* q
   (== 'pea 'pod))
 '())
(check-equal?
 (run* q
   (== q 'pea))
 '(pea))
(check-equal?
 (run* q
   (== 'pea q))
 '(pea))
(check-equal?
 (run* q
   succeed)
 '(_0))
(check-equal?
 (run* q
   (== 'pea 'pea))
 '(_0))
(check-equal?
 (run* q
   (== q q))
 '(_0))
(check-equal?
 (run* q
   (fresh (x)
     (== 'pea q)))
 '(pea))
(check-equal?
 (run* q
   (fresh (x)
     (== 'pea x)))
 '(_0))
(check-equal?
 (run* q
   (fresh (x)
     (== (cons x '()) q)))
 '((_0)))
(check-equal?
 (run* q
   (fresh (x)
     (== `(,x) q)))
 '((_0)))
(check-equal?
 (run* q
   (fresh (x)
     (== x q)))
 '(_0))
(check-equal?
 (run* q
   (== '(((pea)) pod) '(((pea)) pod)))
 '(_0))
(check-equal?
 (run* q
   (== '(((pea)) pod) `(((pea)) ,q)))
 '(pod))
(check-equal?
 (run* q
   (== `(((,q)) pod) '(((pea)) pod)))
 '(pea))
(check-equal?
 (run* q
   (fresh (x)
     (== `(((,q)) pod) `(((,x)) pod))))
 '(_0))
(check-equal?
 (run* q
   (fresh (x)
     (== `(((,q)) ,x) `(((,x)) pod))))
 '(pod))
(check-equal?
 (run* q
   (fresh (x)
     (== `(,x ,x) q)))
 '((_0 _0)))
(check-equal?
 (run* q
   (fresh (x)
     (fresh (y)
       (== `(,q ,y) `((,x ,y) ,x)))))
 '((_0 _0)))
(check-equal?
 (run* q
   (fresh (x)
     (fresh (y)
       (== `(,x ,y) q))))
 '((_0 _1)))
(check-equal?
 (run* s
   (fresh (t)
     (fresh (u)
       (== `(,t ,u) s))))
 '((_0 _1)))
(check-equal?
 (run* q
   (fresh (x)
     (fresh (y)
       (== `(,x ,y ,x) q))))
 '((_0 _1 _0)))
;; 50-59, 61-65.
(check-equal?
 (run* q
   (conj2 succeed succeed))
 '(_0))
(check-equal?
 (run* q
   (conj2 succeed (== 'corn q)))
 '(corn))
(check-equal?
 (run* q
   (conj2 fail (== 'corn q)))
 '())
(check-equal?
 (run* q
   (conj2 (== 'corn q) (== 'meal q)))
 '())
(check-equal?
 (run* q
   (conj2 (== 'corn q) (== 'corn q)))
 '(corn))
(check-equal?
 (run* q
   (disj2 fail fail))
 '())
(check-equal?
 (run* q
   (disj2 (== 'olive q) fail))
 '(olive))
(check-equal?
 (run* q
   (disj2 fail (== 'oil q)))
 '(oil))
(check-equal?
 (run* q
   (disj2 (== 'olive q) (== 'oil q)))
 '(olive oil))
(check-equal?
 (run* q
   (fresh (x)
     (fresh (y)
       (disj2 (== `(,x ,y) q) (== `(,y ,x) q)))))
 '((_0 _1) (_0 _1)))
(check-equal?
 (run* x
   (disj2 (== 'oil x) (== 'olive x)))
 '(oil olive))
(check-equal?
 (run* x
   (disj2 (conj2 (== 'olive x) fail) (== 'oil x)))
 '(oil))
(check-equal?
 (run* x
   (disj2 (conj2 (== 'olive x) succeed) (== 'oil x)))
 '(olive oil))
(check-equal?
 (run* x
   (disj2 (== 'oil x) (conj2 (== 'olive x) succeed)))
 '(oil olive))
(check-equal?
 (run* x
   (disj2 (conj2 (== 'virgin x) fail)
          (disj2 (== 'olive x) (disj2 succeed (== 'oil x)))))
 '(olive _0 oil))
;; 67-70, 72-73, 75-78, 80-81.
(check-equal?
 (run* r
   (fresh (x)
     (fresh (y)
       (conj2 (== 'split x) (conj2 (== 'pea y) (== `(,x ,y) r))))))
 '((split pea)))
(check-equal?
 (run* r
   (fresh (x)
     (fresh (y)
       (conj2 (conj2 (== 'split x) (== 'pea y)) (== `(,x ,y) r)))))
 '((split pea)))
(check-equal?
 (run* r
   (fresh (x y)
     (conj2 (conj2 (== 'split x) (== 'pea y)) (== `(,x ,y) r))))
 '((split pea)))
(check-equal?
 (run* (r x y)
   (conj2 (conj2 (== 'split x) (== 'pea y)) (== `(,x ,y) r)))
 '(((split pea) split pea)))
(check-equal?
 (run* (x y)
   (conj2 (== 'split x) (== 'pea y)))
 '((split pea)))
(check-equal?
 (run* (x y)
   (disj2 (conj2 (== 'split x) (== 'pea y)) (conj2 (== 'red x) (== 'bean y))))
 '((split pea) (red bean)))
(check-equal?
 (run* r
   (fresh (x y)
     (conj2
      (disj2 (conj2 (== 'split x) (== 'pea y)) (conj2 (== 'red x) (== 'bean y)))
      (== `(,x ,y soup) r))))
 '((split pea soup) (red bean soup)))
(check-equal?
 (run* r
   (fresh (x y)
     (disj2 (conj2 (== 'split x) (== 'pea y)) (conj2 (== 'red x) (== 'bean y)))
     (== `(,x ,y soup) r)))
 '((split pea soup) (red bean soup)))
(check-equal?
 (run* (x y z)
   (conj2
    (disj2 (conj2 (== 'split x) (== 'pea y)) (conj2 (== 'red x) (== 'bean y)))
    (== 'soup z)))
 '((split pea soup) (red bean soup)))
(check-equal?
 (run* (x y z)
   (disj2 (conj2 (== 'split x) (== 'pea y)) (conj2 (== 'red x) (== 'bean y)))
   (== 'soup z))
 '((split pea soup) (red bean soup)))
(check-equal?
 (run* (x y)
   (== 'split x)
   (== 'pea y))
 '((split pea)))
;; 82-91.
(defrel (teacupo t)
  (disj2 (== 'tea t) (== 'cup t)))
(check-equal?
 (run* x
   (teacupo x))
 '(tea cup))
(check-equal?
 (run* (x y)
   (disj2 (conj2 (teacupo x) (== #t y)) (conj2 (== #f x) (== #t y))))
 '((#f #t) (tea #t) (cup #t)))
(check-equal?
 (run* (x y)
   (teacupo x)
   (teacupo y))
 '((tea tea) (tea cup) (cup tea) (cup cup)))
(check-equal?
 (run* (x y)
   (teacupo x)
   (teacupo x))
 '((tea _0) (cup _0)))
(check-equal?
 (run* (x y)
   (disj2 (conj2 (teacupo x) (teacupo x)) (conj2 (== #f x) (teacupo y))))
 '((#f tea) (#f cup) (tea _0) (cup _0)))
(check-equal?
 (run* (x y)
   (conde
     [(teacupo x) (teacupo x)]
     [(== #f x) (teacupo y)]))
 '((#f tea) (#f cup) (tea _0) (cup _0)))
(check-equal?
 (run* (x y)
   (conde
     [(== 'split x) (== 'pea y)]
     [(== 'red x) (== 'bean y)]))
 '((split pea) (red bean)))
(check-equal?
 (run* x
   (conde
     [(== 'olive x) fail]
     [(== 'oil x)]))
 '(oil))
(check-equal?
 (run* (x y)
   (conde
     [(fresh (z)
        (== 'lentil z))]
     [(== x y)]))
 '((_0 _1) (_0 _0)))
(check-equal?
 (run* (x y)
   (conde
     [(== 'split x) (== 'pea y)]
     [(== 'red x) (== 'bean y)]
     [(== 'green x) (== 'lentil y)]))
 '((split pea) (red bean) (green lentil)))
