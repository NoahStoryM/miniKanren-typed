;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 9.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 9")

;; 1-4: goal success/failure is observed with run*.
(check-equal?
 (run* q
   (conda
     [fail succeed]
     [succeed fail]))
 '())
(check-equal?
 (run* q
   (conda
     [fail succeed]
     [succeed succeed]))
 '(_0))
(check-equal?
 (run* q
   (conda
     [succeed fail]
     [succeed succeed]))
 '())
(check-equal?
 (run* q
   (conda
     [succeed succeed]
     [succeed fail]))
 '(_0))
;; 5, 7-9, 11-15, 17-18.
(check-equal?
 (run* x
   (conda
     [(== 'olive x) succeed]
     [succeed (== 'oil x)]))
 '(olive))
(check-equal?
 (run* x
   (conda
     [(== 'virgin x) fail]
     [(== 'olive x) succeed]
     [succeed (== 'oil x)]))
 '())
(check-equal?
 (run* q
   (fresh (x y)
     (== 'split x)
     (== 'pea y)
     (conda
       [(== 'split x) (== x y)]
       [succeed succeed])))
 '())
(check-equal?
 (run* q
   (fresh (x y)
     (== 'split x)
     (== 'pea y)
     (conda
       [(== x y) (== 'split x)]
       [succeed succeed])))
 '(_0))

(defrel (not-pastao x)
  (conda
    [(== 'pasta x) fail]
    [succeed succeed]))
(check-equal?
 (run* x
   (conda
     [(not-pastao x) fail]
     [(== 'spaghetti x) succeed]))
 '(spaghetti))
(check-equal?
 (run* x
   (== 'spaghetti x)
   (conda
     [(not-pastao x) fail]
     [(== 'spaghetti x) succeed]))
 '())
;; 13: nonterminating: (run* q (conda ((alwayso) succeed) (succeed fail)))
(check-equal?
 (run* q
   (condu
     [(alwayso) succeed]
     [succeed fail]))
 '(_0))
;; 15: nonterminating: (run* q (condu (succeed (alwayso)) (succeed fail)))
;; 17: nonterminating: (run 1 q (conda ((alwayso) succeed) (succeed fail)) fail)
(check-equal?
 (run 1 q
   (condu
     [(alwayso) succeed]
     [succeed fail])
   fail)
 '())
;; 20-27. Higher-order relations give their goal arguments a type.
(defrel (onceo [g : Goal])
  (condu
    [g succeed]
    [succeed fail]))
(check-equal?
 (run* x
   (onceo (teacupo x)))
 '(tea))
(check-equal?
 (run* r
   (conde
     [(teacupo r) succeed]
     [(== #f r) succeed]))
 '(#f tea cup))
(check-equal?
 (run* r
   (conda
     [(teacupo r) succeed]
     [succeed (== #f r)]))
 '(tea cup))
(check-equal?
 (run* r
   (== #f r)
   (conda
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [succeed fail]))
 '(#f))
(check-equal?
 (run* r
   (== #f r)
   (condu
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [succeed fail]))
 '(#f))

(defrel (bumpo n x)
  (conde
    [(== n x)]
    [(fresh (m)
       (minuso n '(1) m)
       (bumpo m x))]))
(check-equal?
 (run* x
   (bumpo '(1 1 1) x))
 '((1 1 1) (0 1 1) (1 0 1) (0 0 1) (1 1) (0 1) (1) ()))

(defrel (gen&test+o i j k)
  (onceo
   (fresh (x y z)
     (pluso x y z)
     (== i x)
     (== j y)
     (== k z))))
(check-equal?
 (run* q
   (gen&test+o '(0 0 1) '(1 1) '(1 1 1)))
 '(_0))
;; 40, 42: nonterminating: (run 1 q (gen&test+o '(0 0 1) '(1 1) '(0 1 1)))
;; 43, 56.
(defrel (enumerate+o r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (pluso i j k)
    (gen&test+o i j k)
    (== `(,i ,j ,k) r)))
(check-equal?
 (run* s
   (enumerate+o s '(1 1)))
 '((() (1 1) (1 1))
   ((1 1) () (1 1))
   ((1 1) (1 1) (0 1 1))
   (() (0 1) (0 1))
   ((1 1) (0 1) (1 0 1))
   (() (1) (1))
   ((1 1) (1) (0 0 1))
   ((1) (1 1) (0 0 1))
   (() () ())
   ((1) (1) (0 1))
   ((1) (0 1) (1 1))
   ((0 1) () (0 1))
   ((1) () (1))
   ((0 1) (0 1) (0 0 1))
   ((0 1) (1 1) (1 0 1))
   ((0 1) (1) (1 1))))
(check-equal?
 (run 1 s
   (enumerate+o s '(1 1 1)))
 '((() (1 1 1) (1 1 1))))
;; 58-59: the inlined and generalized definitions have no new printed query.
;; Keep them scoped so each definition has its book name.
(let ()
  (defrel (enumerate+o r n)
    (fresh (i j k)
      (bumpo n i)
      (bumpo n j)
      (pluso i j k)
      (onceo
       (fresh (x y z)
         (pluso x y z)
         (== i x)
         (== j y)
         (== k z)))
      (== `(,i ,j ,k) r)))
  (void))

(defrel (enumerateo [op : (→ Term Term Term Goal)] r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (onceo
     (fresh (x y z)
       (op x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== `(,i ,j ,k) r)))
