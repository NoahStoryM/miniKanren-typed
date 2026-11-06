(displayln "Test code in chapter 4")
(: mem (→ Term (Listof Term) (Option (Listof Term))))
(define (mem x l)
  (cond
    [(null? l) #f]
    [(eq-car? l x) l]
    [else (mem x (cdr l))]))
(check-equal?
 (run* (out)
   (== (mem 'tofu '(a b tofu d peas e)) out))
 '((tofu d peas e)))

(: memo (→ Term Term Term Goal))
(define (memo x l out)
  (conde
    [(nullo l) fail]
    [(eq-caro l x) (== l out)]
    [else
     (fresh (d)
       (cdro l d)
       (memo x d out))]))
(check-equal?
 (run 1 (out)
   (memo 'tofu '(a b tofu d tofu e) out))
 '((tofu d tofu e)))
(check-equal?
 (run* (r)
   (memo r
     '(a b tofu d tofu e)
     '(tofu d tofu e)))
 '(tofu))
(check-equal?
 (run* (q)
   (memo 'tofu '(tofu e) '(tofu e))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (memo 'tofu '(tofu e) '(tofu))
   (== #t q))
 '())
(check-equal?
 (run* (x)
   (memo 'tofu '(tofu e) `(,x e)))
 '(tofu))
(check-equal?
 (run* (x)
   (memo 'tofu '(tofu e) `(peas ,x)))
 '())
(check-equal?
 (run* (out)
   (fresh (x)
     (memo 'tofu `(a b ,x d tofu e) out)))
 '((tofu d tofu e) (tofu e)))
(check-equal?
 (run 12 (z)
   (fresh (u)
     (memo 'tofu `(a b tofu d tofu e . ,z) u)))
 '(_.0
   _.0
   (tofu . _.0)
   (_.0 tofu . _.1)
   (_.0 _.1 tofu . _.2)
   (_.0 _.1 _.2 tofu . _.3)
   (_.0 _.1 _.2 _.3 tofu . _.4)
   (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
   (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
   (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9)))

(: rembero (→ Term Term Term Goal))
(define (rembero x l out)
  (conde
    [(nullo l) (== '() out)]
    [(eq-caro l x) (cdro l out)]
    [else
     (fresh (a d res)
       (conso a d l)
       (conso a res out)
       (rembero x d res))]))
(check-equal?
 (run 1 (out)
   (fresh (y)
     (rembero 'peas `(a b ,y d peas e) out)))
 '((a b d peas e)))
(check-equal?
 (run* (out)
   (fresh (y z)
     (rembero y `(a b ,y d ,z e) out)))
 '((b a d _.0 e)
   (a b d _.0 e)
   (a b d _.0 e)
   (a b d _.0 e)
   (a b _.0 d e)
   (a b e d _.0)
   (a b _.0 d _.1 e)))
(check-equal?
 (run* (r)
   (fresh (y z)
     (rembero y `(,y d ,z e) `(,y d e))
     (== `(,y ,z) r)))
 '((d d)
   (d d)
   (_.0 _.0)
   (e e)))
(check-equal?
 (run 13 (w)
   (fresh (y z out)
     (rembero y `(a b ,y d ,z . ,w) out)))
 '(_.0
   _.0
   _.0
   _.0
   _.0
   ()
   (_.0 . _.1)
   (_.0)
   (_.0 _.1 . _.2)
   (_.0 _.1)
   (_.0 _.1 _.2 . _.3)
   (_.0 _.1 _.2)
   (_.0 _.1 _.2 _.3 . _.4)))

(: surpriseo (→ Term Goal))
(define (surpriseo s)
  (rembero s '(a b c) '(a b c)))
(check-equal?
 (run* (r)
   (== 'd r)
   (surpriseo r))
 '(d))
(check-equal?
 (run* (r)
   (surpriseo r))
 '(_.0))
(check-equal?
 (run* (r)
   (surpriseo r)
   (== 'b r))
 '(b))
(check-equal?
 (run* (r)
   (== 'b r)
   (surpriseo r))
 '(b))
