(displayln "Test code in chapter 10")
(check-equal?
 (run* (x)
   (conda
     [(== 'olive x) succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '(olive))
(check-equal?
 (run* (x)
   (conda
     [(== 'virgin x) fail]
     [(== 'olive x) succeed]
     [(== 'oil x) succeed]
     [else fail]))
 '())
(check-equal?
 (run* (q)
   (fresh (x y)
     (== 'split x)
     (== 'pea y)
     (conda
       [(== 'split x) (== x y)]
       [else succeed]))
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (fresh (x y)
     (== 'split x)
     (== 'pea y)
     (conda
       [(== x y) (== 'split x)]
       [else succeed]))
   (== #t q))
 '(#t))

(: not-pastao (→ Term Goal))
(define (not-pastao x)
  (conda
    [(== 'pasta x) fail]
    [else succeed]))
(check-equal?
 (run* (x)
   (conda
     [(not-pastao x) fail]
     [else (== 'spaghetti x)]))
 '(spaghetti))
(check-equal?
 (run* (x)
   (== 'spaghetti x)
   (conda
     [(not-pastao x) fail]
     [else (== 'spaghetti x)]))
 '())

(check-equal?
 (run* (q)
   (condu
     [alwayso succeed]
     [else fail])
   (== #t q))
 '(#t))
(check-equal?
 (run 1 (q)
   (condu
     [alwayso succeed]
     [else fail])
   fail
   (== #t q))
 '())

(: onceo (→ Goal Goal))
(define (onceo g)
  (condu
    [g succeed]
    [else fail]))
(check-equal?
 (run* (x)
   (onceo (teacupo x)))
 '(tea))
(check-equal?
 (run 1 (q)
   (onceo (salo nevero))
   fail)
 '())
(check-equal?
 (run* (r)
   (conde
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [else fail]))
 '(tea cup #f))
(check-equal?
 (run* (r)
   (conda
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [else fail]))
 '(tea cup))
(check-equal?
 (run* (r)
   (== #f r)
   (conda
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [else fail]))
 '(#f))
(check-equal?
 (run* (r)
   (== #f r)
   (condu
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [else fail]))
 '(#f))

(: bumpo (→ Term Term Goal))
(define (bumpo n x)
  (conde
    [(== n x) succeed]
    [else
     (fresh (m)
       (-o n '(1) m)
       (bumpo m x))]))
(check-equal?
 (run* (x)
   (bumpo '(1 1 1) x))
 '((1 1 1)
   (0 1 1)
   (1 0 1)
   (0 0 1)
   (1 1)
   (0 1)
   (1)
   ()))

(: gen&testo (→ (→ Term Term Term Goal) Term Term Term Goal))
(define (gen&testo op i j k)
  (onceo
   (fresh (x y z)
     (op x y z)
     (== i x)
     (== j y)
     (== k z))))
(check-equal?
 (run* (q)
   (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
   (== #t q))
 '(#t))

(: enumerateo (→ (→ Term Term Term Goal) Term Term Goal))
(define (enumerateo op r n)
  (fresh (i j k)
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (gen&testo op i j k)
    (== `(,i ,j ,k) r)))
(check-equal?
 (run* (s)
   (enumerateo +o s '(1 1)))
 '(((1 1) (1 1) (0 1 1))
   ((1 1) (0 1) (1 0 1))
   ((1 1) (1) (0 0 1))
   ((1 1) () (1 1))
   ((0 1) (1 1) (1 0 1))
   ((0 1) (0 1) (0 0 1))
   ((0 1) (1) (1 1))
   ((0 1) () (0 1))
   ((1) (1 1) (0 0 1))
   ((1) (0 1) (1 1))
   ((1) (1) (0 1))
   ((1) () (1))
   (() (1 1) (1 1))
   (() (0 1) (0 1))
   (() (1) (1))
   (() () ())))
