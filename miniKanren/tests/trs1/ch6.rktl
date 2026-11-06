(displayln "Test code in chapter 6")
(: anyo (→ Goal Goal))
(define (anyo g)
  (conde
    [g succeed]
    [else (anyo g)]))
(define nevero (anyo fail))
(define alwayso (anyo succeed))
(check-equal?
 (run 1 (q)
   fail
   nevero)
 '())
(check-equal?
 (run 1 (q)
   alwayso
   (== #t q))
 '(#t))
(check-equal?
 (run 5 (q)
   alwayso
   (== #t q))
 '(#t #t #t #t #t))

(: salo (→ Goal Goal))
(define (salo g)
  (conde
    [succeed succeed]
    [else g]))
(check-equal?
 (run 1 (q)
   (salo alwayso)
   (== #t q))
 '(#t))
(check-equal?
 (run 1 (q)
   (condi
     [(== #f q) alwayso]
     [else (== #t q)])
   (== #t q))
 '(#t))
(check-equal?
 (run 5 (q)
   (condi
     [(== #f q) alwayso]
     [else (anyo (== #t q))])
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (r)
   (condi
     [(teacupo r) succeed]
     [(== #f r) succeed]
     [else fail]))
 '(tea #f cup))
(check-equal?
 (run 5 (q)
   (condi
     [(== #f q) alwayso]
     [(== #t q) alwayso]
     [else fail])
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (q)
   (conde
     [alwayso succeed]
     [else nevero])
   (== #t q))
 '(#t #t #t #t #t))

(check-equal?
 (run 1 (q)
   (alli
     (conde
       [(== #f q) succeed]
       [else (== #t q)])
     alwayso)
   (== #t q))
 '(#t))
(check-equal?
 (run 5 (q)
   (alli
     (conde
       [(== #f q) succeed]
       [else (== #t q)])
     alwayso)
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (q)
   (alli
     (conde
       [(== #t q) succeed]
       [else (== #f q)])
     alwayso)
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (q)
   (all
     (conde
       [succeed succeed]
       [else nevero])
     alwayso)
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (q)
   (alli
     (condi
       [(== #f q) succeed]
       [else (== #t q)])
     alwayso)
   (== #t q))
 '(#t #t #t #t #t))
(check-equal?
 (run 5 (q)
   (all
     (condi
       [succeed succeed]
       [else nevero])
     alwayso)
   (== #t q))
 '(#t #t #t #t #t))
