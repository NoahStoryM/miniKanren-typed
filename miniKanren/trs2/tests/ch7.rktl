;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 7.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 7")

;; 6, 8-9, 11-13, 15-17, 39-41, 77-80, 83-87, 90, 94.
(check-equal?
 (run* (x y)
   (bit-xoro x y 0))
 '((0 0) (1 1)))
(check-equal?
 (run* (x y)
   (bit-xoro x y 1))
 '((0 1) (1 0)))
(check-equal?
 (run* (x y r)
   (bit-xoro x y r))
 '((0 0 0) (0 1 1) (1 0 1) (1 1 0)))
(check-equal?
 (run* (x y)
   (bit-ando x y 1))
 '((1 1)))
(check-equal?
 (run* r
   (half-addero 1 1 r 1))
 '(0))
(check-equal?
 (run* (x y r c)
   (half-addero x y r c))
 '((0 0 0 0) (0 1 1 0) (1 0 1 0) (1 1 0 1)))
(check-equal?
 (run* (r c)
   (full-addero 0 1 1 r c))
 '((0 1)))
(check-equal?
 (run* (r c)
   (full-addero 1 1 1 r c))
 '((1 1)))
(check-equal?
 (run* (b x y r c)
   (full-addero b x y r c))
 '((0 0 0 0 0)
   (1 0 0 1 0)
   (0 1 0 1 0)
   (1 1 0 0 1)
   (0 0 1 1 0)
   (1 0 1 0 1)
   (0 1 1 0 1)
   (1 1 1 1 1)))
(check-equal?
 (build-num 0)
 '())
(check-equal?
 (build-num 36)
 '(0 0 1 0 0 1))
(check-equal?
 (build-num 19)
 '(1 1 0 0 1))
(check-equal?
 (run* q
   (poso '(0 1 1)))
 '(_0))
(check-equal?
 (run* q
   (poso '(1)))
 '(_0))
(check-equal?
 (run* q
   (poso '()))
 '())
(check-equal?
 (run* r
   (poso r))
 '((_0 . _1)))
(check-equal?
 (run* q
   (>1o '(0 1 1)))
 '(_0))
(check-equal?
 (run* q
   (>1o '(0 1)))
 '(_0))
(check-equal?
 (run* q
   (>1o '(1)))
 '())
(check-equal?
 (run* q
   (>1o '()))
 '())
(check-equal?
 (run* r
   (>1o r))
 '((_0 _1 . _2)))
(check-equal?
 (run 3 (x y r)
   (addero 0 x y r))
 '((_0 () _0) (() (_0 . _1) (_0 . _1)) ((1) (1) (0 1))))
(check-equal?
 (run 19 (x y r)
   (addero 0 x y r))
 '((_0 () _0)
   (() (_0 . _1) (_0 . _1))
   ((1) (1) (0 1))
   ((1) (0 _0 . _1) (1 _0 . _1))
   ((1) (1 1) (0 0 1))
   ((0 1) (0 1) (0 0 1))
   ((1) (1 0 _0 . _1) (0 1 _0 . _1))
   ((0 _0 . _1) (1) (1 _0 . _1))
   ((1) (1 1 1) (0 0 0 1))
   ((1 1) (0 1) (1 0 1))
   ((1 1) (1) (0 0 1))
   ((1) (1 1 0 _0 . _1) (0 0 1 _0 . _1))
   ((1) (1 1 1 1) (0 0 0 0 1))
   ((1) (1 1 1 0 _0 . _1) (0 0 0 1 _0 . _1))
   ((1 0 _0 . _1) (1) (0 1 _0 . _1))
   ((1) (1 1 1 1 1) (0 0 0 0 0 1))
   ((0 1) (1 1) (1 0 1))
   ((1 1 1) (1) (0 0 0 1))
   ((1 1) (1 1) (0 1 1))))
;; 106, 112, 115, 117-119.
(check-equal?
 (run* s
   (gen-addero 1 '(0 1 1) '(1 1) s))
 '((0 1 0 1)))
(check-equal?
 (run* (x y)
   (addero 0 x y '(1 0 1)))
 '(((1 0 1) ())
   (() (1 0 1))
   ((1) (0 0 1))
   ((0 0 1) (1))
   ((1 1) (0 1))
   ((0 1) (1 1))))
(check-equal?
 (run* (x y)
   (pluso x y '(1 0 1)))
 '(((1 0 1) ())
   (() (1 0 1))
   ((1) (0 0 1))
   ((0 0 1) (1))
   ((1 1) (0 1))
   ((0 1) (1 1))))
(check-equal?
 (run* q
   (minuso '(0 0 0 1) '(1 0 1) q))
 '((1 1)))
(check-equal?
 (run* q
   (minuso '(0 1 1) '(0 1 1) q))
 '(()))
(check-equal?
 (run* q
   (minuso '(0 1 1) '(0 0 0 1) q))
 '())
;; 120-125: lengtho is a list relation using the imported arithmetic.
(defrel (lengtho l n)
  (conde
    [(nullo l) (== '() n)]
    [(fresh (d res)
       (cdro l d)
       (pluso '(1) res n)
       (lengtho d res))]))
(check-equal?
 (run 1 n
   (lengtho '(jicama rhubarb guava) n))
 '((1 1)))
(check-equal?
 (run* ls
   (lengtho ls '(1 0 1)))
 '((_0 _1 _2 _3 _4)))
(check-equal?
 (run* q
   (lengtho '(1 0 1) 3))
 '())
(check-equal?
 (run 3 q
   (lengtho q q))
 '(() (1) (0 1)))
;; 125: nonterminating: (run 4 q (lengtho q q))
