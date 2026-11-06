;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 5.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 5")

(: mem (→ Term (Listof Term) (U False (Listof Term))))
(define (mem x l)
  (cond
    [(null? l) #f]
    [(equal? (car l) x) l]
    [#t (mem x (cdr l))]))

(defrel (memo x l out)
  (conde
    [(caro l x) (== l out)]
    [(fresh (d)
       (cdro l d)
       (memo x d out))]))

(: rember (→ Term (Listof Term) (Listof Term)))
(define (rember x l)
  (cond
    [(null? l) '()]
    [(equal? (car l) x) (cdr l)]
    [#t (cons (car l) (rember x (cdr l)))]))

(defrel (rembero x l out)
  (conde
    [(nullo l) (== '() out)]
    [(conso x out l)]
    [(fresh (a d res)
       (conso a d l)
       (conso a res out)
       (rembero x d res))]))
;; 1-3, 5-11, 13-15, 18-19, 24, 26-28, 48, 56, 61.
(check-equal?
 (mem 'fig '(roll okra fig beet roll pea))
 '(fig beet roll pea))
(check-equal?
 (mem 'fig '(roll okra beet beet roll pea))
 '#f)
(check-equal?
 (mem 'roll (assert (mem 'fig '(roll okra fig beet roll pea)) list?))
 '(roll pea))
(check-equal?
 (run* q
   (memo 'fig '(pea) '(pea)))
 '())
(check-equal?
 (run* out
   (memo 'fig '(fig) out))
 '((fig)))
(check-equal?
 (run* out
   (memo 'fig '(fig pea) out))
 '((fig pea)))
(check-equal?
 (run* r
   (memo r '(roll okra fig beet fig pea) '(fig beet fig pea)))
 '(fig))
(check-equal?
 (run* x
   (memo 'fig '(fig pea) `(pea ,x)))
 '())
(check-equal?
 (run* x
   (memo 'fig '(fig pea) `(,x pea)))
 '(fig))
(check-equal?
 (run* out
   (memo 'fig '(beet fig pea) out))
 '((fig pea)))
(check-equal?
 (run 1 out
   (memo 'fig '(fig fig pea) out))
 '((fig fig pea)))
(check-equal?
 (run* out
   (memo 'fig '(fig fig pea) out))
 '((fig fig pea) (fig pea)))
(check-equal?
 (run* out
   (fresh (x)
     (memo 'fig `(a ,x c fig e) out)))
 '((fig c fig e) (fig e)))
(check-equal?
 (run 5 (x y)
   (memo 'fig `(fig d fig e . ,y) x))
 '(((fig d fig e . _0) _0)
   ((fig e . _0) _0)
   ((fig . _0) (fig . _0))
   ((fig . _0) (_1 fig . _0))
   ((fig . _0) (_1 _2 fig . _0))))
(check-equal?
 (rember 'pea '(a b pea d pea e))
 '(a b d pea e))
(check-equal?
 (run* out
   (rembero 'pea '(pea) out))
 '(() (pea)))
(check-equal?
 (run* out
   (rembero 'pea '(pea pea) out))
 '((pea) (pea) (pea pea)))
(check-equal?
 (run* out
   (fresh (y z)
     (rembero y `(a b ,y d ,z e) out)))
 '((b a d _0 e)
   (a b d _0 e)
   (a b d _0 e)
   (a b d _0 e)
   (a b _0 d e)
   (a b e d _0)
   (a b _0 d _1 e)))
(check-equal?
 (run* (y z)
   (rembero y `(,y d ,z e) `(,y d e)))
 '((d d) (d d) (_0 _0) (e e)))
(check-equal?
 (run 4 (y z w out)
   (rembero y `(,z . ,w) out))
 '((_0 _0 _1 _1)
   (_0 _1 () (_1))
   (_0 _1 (_0 . _2) (_1 . _2))
   (_0 _1 (_2) (_1 _2))))
(check-equal?
 (run 5 (y z w out)
   (rembero y `(,z . ,w) out))
 '((_0 _0 _1 _1)
   (_0 _1 () (_1))
   (_0 _1 (_0 . _2) (_1 . _2))
   (_0 _1 (_2) (_1 _2))
   (_0 _1 (_2 _0 . _3) (_1 _2 . _3))))
