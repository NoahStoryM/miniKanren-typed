(displayln "Test code in chapter 2")
(check-equal?
 (run* (r)
   (fresh (y x)
     (== `(,x ,y) r)))
 '((_.0 _.1)))
(check-equal?
 (run* (r)
   (fresh (v w)
     (== (let ([x v] [y w]) `(,x ,y)) r)))
 '((_.0 _.1)))

(: caro (→ Term Term Goal))
(define (caro p a) (fresh (d) (== (cons a d) p)))
(check-equal?
 (run* (r)
   (caro '(a c o r n) r))
 '(a))
(check-equal?
 (run* (q)
   (caro '(a c o r n) 'a)
   (== #t q))
 '(#t))
(check-equal?
 (run* (r)
   (fresh (x y)
     (caro `(,r ,y) x)
     (== 'pear x)))
 '(pear))
(check-equal?
 (run* (r)
   (fresh (x y)
     (caro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '((grape a)))

(: cdro (→ Term Term Goal))
(define (cdro p d) (fresh (a) (== (cons a d) p)))
(check-equal?
 (run* (r)
   (fresh (v)
     (cdro '(a c o r n) v)
     (caro v r)))
 '(c))
(check-equal?
 (run* (r)
   (fresh (x y)
     (cdro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '(((raisin pear) a)))

(check-equal?
 (run* (q)
   (cdro '(a c o r n) '(c o r n))
   (== #t q))
 '(#t))
(check-equal?
 (run* (x)
   (cdro '(c o r n) `(,x r n)))
 '(o))
(check-equal?
 (run* (l)
   (fresh (x)
     (cdro l '(c o r n))
     (caro l x)
     (== 'a x)))
 '((a c o r n)))

(: conso (→ Term Term Term Goal))
(define (conso a d p) (== (cons a d) p))
(check-equal?
 (run* (l)
   (conso '(a b c) '(d e) l))
 '(((a b c) d e)))
(check-equal?
 (run* (x)
   (conso x '(a b c) '(d a b c)))
 '(d))
(check-equal?
 (run* (r)
   (fresh (x y z)
     (== `(e a d ,x) r)
     (conso y `(a ,z c) r)))
 '((e a d c)))
(check-equal?
 (run* (x)
   (conso x `(a ,x c) `(d a ,x c)))
 '(d))
(check-equal?
 (run* (l)
   (fresh (x)
     (== `(d a ,x c) l)
     (conso x `(a ,x c) l)))
 '((d a d c)))
(check-equal?
 (run* (l)
   (fresh (x)
     (conso x `(a ,x c) l)
     (== `(d a ,x c) l)))
 '((d a d c)))

(check-equal?
 (run* (l)
   (fresh (d x y w s)
     (conso w '(a n s) s)
     (cdro l s)
     (caro l x)
     (== 'b x)
     (cdro l d)
     (caro d y)
     (== 'e y)))
 '((b e a n s)))

(: nullo (→ Term Goal))
(define (nullo x) (== '() x))
(check-equal?
 (run* (q)
   (nullo '(grape raisin pear))
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (nullo '())
   (== #t q))
 '(#t))
(check-equal?
 (run* (x)
   (nullo x))
 '(()))

(: eqo (→ Term Term Goal))
(define (eqo x y) (== x y))
(check-equal?
 (run* (q)
   (eqo 'pear 'plum)
   (== #t q))
 '())
(check-equal?
 (run* (r)
   (fresh (x y)
     (== (cons x (cons y 'salad)) r)))
 '((_.0 _.1 . salad)))

(: pairo (→ Term Goal))
(define (pairo p) (fresh (a d) (== (cons a d) p)))
(check-equal?
 (run* (q)
   (pairo (cons q q))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (pairo '())
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (pairo 'pair)
   (== #t q))
 '())
(check-equal?
 (run* (x)
   (pairo x))
 '((_.0 . _.1)))
(check-equal?
 (run* (r)
   (pairo (cons r 'pear)))
 '(_.0))
