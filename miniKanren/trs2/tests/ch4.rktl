;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 4.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 4")

;; 1-6. The ordinary append accepts an arbitrary final term.
(: append (→ (Listof Term) Term Term))
(define (append l t)
  (cond
    [(null? l) t]
    [#t (cons (car l) (append (cdr l) t))]))
(check-equal?
 (append '(a b c) '(d e))
 '(a b c d e))
(check-equal?
 (append '(a b c) '())
 '(a b c))
(check-equal?
 (append '() '(d e))
 '(d e))
;; 4: undefined (not a proper first list): (append 'a '(d e))
(check-equal?
 (append '(d e) 'a)
 '(d e . a))
;; 13-39: retain the earlier goal order in a local scope.
(let ()
  (defrel (appendo l t out)
    (conde
      [(nullo l) (== t out)]
      [(fresh (a d res)
         (conso a d l)
         (appendo d t res)
         (conso a res out))]))
  (check-equal?
   (run 6 x
     (fresh (y z)
       (appendo x y z)))
   '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3) (_0 _1 _2 _3 _4)))
  (check-equal?
   (run 6 y
     (fresh (x z)
       (appendo x y z)))
   '(_0 _0 _0 _0 _0 _0))
  (check-equal?
   (run 6 z
     (fresh (x y)
       (appendo x y z)))
   '(_0
     (_0 . _1)
     (_0 _1 . _2)
     (_0 _1 _2 . _3)
     (_0 _1 _2 _3 . _4)
     (_0 _1 _2 _3 _4 . _5)))
  (check-equal?
   (run 6 (x y z)
     (appendo x y z))
   '((() _0 _0)
     ((_0) _1 (_0 . _1))
     ((_0 _1) _2 (_0 _1 . _2))
     ((_0 _1 _2) _3 (_0 _1 _2 . _3))
     ((_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4))
     ((_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5))))
  (check-equal?
   (run* x
     (appendo '(cake) '(tastes yummy) x))
   '((cake tastes yummy)))
  (check-equal?
   (run* x
     (fresh (y)
       (appendo `(cake & ice ,y) '(tastes yummy) x)))
   '((cake & ice _0 tastes yummy)))
  (check-equal?
   (run* x
     (fresh (y)
       (appendo '(cake & ice cream) y x)))
   '((cake & ice cream . _0)))
  (check-equal?
   (run 1 x
     (fresh (y)
       (appendo `(cake & ice . ,y) '(d t) x)))
   '((cake & ice d t)))
  (check-equal?
   (run 5 x
     (fresh (y)
       (appendo `(cake & ice . ,y) '(d t) x)))
   '((cake & ice d t)
     (cake & ice _0 d t)
     (cake & ice _0 _1 d t)
     (cake & ice _0 _1 _2 d t)
     (cake & ice _0 _1 _2 _3 d t)))
  (check-equal?
   (run 5 y
     (fresh (x)
       (appendo `(cake & ice . ,y) '(d t) x)))
   '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3)))
  (check-equal?
   (append '(cake & ice _0 _1 _2) '(d t))
   '(cake & ice _0 _1 _2 d t))
  (check-equal?
   (run 5 x
     (fresh (y)
       (appendo `(cake & ice . ,y) `(d t . ,y) x)))
   '((cake & ice d t)
     (cake & ice _0 d t _0)
     (cake & ice _0 _1 d t _0 _1)
     (cake & ice _0 _1 _2 d t _0 _1 _2)
     (cake & ice _0 _1 _2 _3 d t _0 _1 _2 _3)))
  (check-equal?
   (run* x
     (fresh (z)
       (appendo '(cake & ice cream) `(d t . ,z) x)))
   '((cake & ice cream d t . _0)))
  (check-equal?
   (run 6 x
     (fresh (y)
       (appendo x y '(cake & ice d t))))
   '(() (cake) (cake &) (cake & ice) (cake & ice d) (cake & ice d t)))
  (check-equal?
   (run 6 y
     (fresh (x)
       (appendo x y '(cake & ice d t))))
   '((cake & ice d t) (& ice d t) (ice d t) (d t) (t) ()))
  (check-equal?
   (run 6 (x y)
     (appendo x y '(cake & ice d t)))
   '((() (cake & ice d t))
     ((cake) (& ice d t))
     ((cake &) (ice d t))
     ((cake & ice) (d t))
     ((cake & ice d) (t))
     ((cake & ice d t) ())))
  ;; 39: nonterminating: (run 7 (x y) (appendo x y '(cake & ice d t)))
)
;; 41-44.
(defrel (appendo l t out)
  (conde
    [(nullo l) (== t out)]
    [(fresh (a d res)
       (conso a d l)
       (conso a res out)
       (appendo d t res))]))
(check-equal?
 (run* (x y)
   (appendo x y '(cake & ice d t)))
 '((() (cake & ice d t))
   ((cake) (& ice d t))
   ((cake &) (ice d t))
   ((cake & ice) (d t))
   ((cake & ice d) (t))
   ((cake & ice d t) ())))

(defrel (swappendo l t out)
  (conde
    [(fresh (a d res)
       (conso a d l)
       (conso a res out)
       (swappendo d t res))]
    [(nullo l) (== t out)]))
(check-equal?
 (run* (x y)
   (swappendo x y '(cake & ice d t)))
 '((() (cake & ice d t))
   ((cake) (& ice d t))
   ((cake &) (ice d t))
   ((cake & ice) (d t))
   ((cake & ice d) (t))
   ((cake & ice d t) ())))
;; 45-54.
(: unwrap (→ Term Term))
(define (unwrap x)
  (cond
    [(pair? x) (unwrap (car x))]
    [#t x]))
(check-equal?
 (unwrap '((((pizza)))))
 'pizza)
(check-equal?
 (unwrap '((((pizza pie) with)) garlic))
 'pizza)

(defrel (unwrapo x out)
  (conde
    [(fresh (a)
       (caro x a)
       (unwrapo a out))]
    [(== x out)]))
(check-equal?
 (run* x
   (unwrapo '(((pizza))) x))
 '((((pizza))) ((pizza)) (pizza) pizza))
(check-equal?
 (run 1 x
   (unwrapo x 'pizza))
 '(pizza))
(check-equal?
 (run 1 x
   (unwrapo `((,x)) 'pizza))
 '(pizza))
(check-equal?
 (run 5 x
   (unwrapo x 'pizza))
 '(pizza
   (pizza . _0)
   ((pizza . _0) . _1)
   (((pizza . _0) . _1) . _2)
   ((((pizza . _0) . _1) . _2) . _3)))
(check-equal?
 (run 5 x
   (unwrapo x '((pizza))))
 '(((pizza))
   (((pizza)) . _0)
   ((((pizza)) . _0) . _1)
   (((((pizza)) . _0) . _1) . _2)
   ((((((pizza)) . _0) . _1) . _2) . _3)))
(check-equal?
 (run 5 x
   (unwrapo `((,x)) 'pizza))
 '(pizza
   (pizza . _0)
   ((pizza . _0) . _1)
   (((pizza . _0) . _1) . _2)
   ((((pizza . _0) . _1) . _2) . _3)))
