;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 2.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 2")

(defrel (caro p a) (fresh (d) (== (cons a d) p)))
(defrel (cdro p d) (fresh (a) (== (cons a d) p)))
(defrel (conso a d p) (== `(,a . ,d) p))
(defrel (nullo x) (== '() x))
(defrel (pairo p) (fresh (a d) (conso a d p)))
(: singleton? (-> Term Boolean))
(define (singleton? l) (cond ((pair? l) (null? (cdr l))) (#t #f)))
(defrel (singletono l) (fresh (d) (cdro l d) (nullo d)))
;; 1-5, 7-8, 10-12, 14-24, 27-32, 36-37, 41-45, 47-51, 58, 60-62.
(check-equal? (car '(grape raisin pear)) 'grape)
(check-equal? (car '(a c o r n)) 'a)
(check-equal? (run* q (caro '(a c o r n) q)) '(a))
(check-equal? (run* q (caro '(a c o r n) 'a)) '(_0))
(check-equal? (run* r (fresh (x y) (caro `(,r ,y) x) (== 'pear x))) '(pear))
(check-equal? (cons (car '(grape raisin pear)) (car '((a) (b) (c)))) '(grape a))
(check-equal?
 (run* r
   (fresh (x y)
     (caro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '((grape a)))
(check-equal? (cdr '(grape raisin pear)) '(raisin pear))
(check-equal? (car (cdr (cdr '(a c o r n)))) 'o)
(check-equal?
 (run* r (fresh (v) (cdro '(a c o r n) v) (fresh (w) (cdro v w) (caro w r))))
 '(o))
(check-equal?
 (cons (cdr '(grape raisin pear)) (car '((a) (b) (c))))
 '((raisin pear) a))
(check-equal?
 (run* r
   (fresh (x y)
     (cdro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '(((raisin pear) a)))
(check-equal? (run* q (cdro '(a c o r n) '(c o r n))) '(_0))
(check-equal? (run* x (cdro '(c o r n) `(,x r n))) '(o))
(check-equal?
 (run* l (fresh (x) (cdro l '(c o r n)) (caro l x) (== 'a x)))
 '((a c o r n)))
(check-equal? (run* l (conso '(a b c) '(d e) l)) '(((a b c) d e)))
(check-equal? (run* x (conso x '(a b c) '(d a b c))) '(d))
(check-equal?
 (run* r (fresh (x y z) (== `(e a d ,x) r) (conso y `(a ,z c) r)))
 '((e a d c)))
(check-equal? (run* x (conso x `(a ,x c) `(d a ,x c))) '(d))
(check-equal?
 (run* l (fresh (x) (== `(d a ,x c) l) (conso x `(a ,x c) l)))
 '((d a d c)))
(check-equal?
 (run* l (fresh (x) (conso x `(a ,x c) l) (== `(d a ,x c) l)))
 '((d a d c)))
(check-equal?
 (run* l
   (fresh (d t x y w)
     (conso w '(n u s) t)
     (cdro l t)
     (caro l x)
     (== 'b x)
     (cdro l d)
     (caro d y)
     (== 'o y)))
 '((b o n u s)))
(check-equal? (null? '(grape raisin pear)) '#f)
(check-equal? (null? '()) '#t)
(check-equal? (run* q (nullo '(grape raisin pear))) '())
(check-equal? (run* q (nullo '())) '(_0))
(check-equal? (run* x (nullo x)) '(()))
(check-equal? (pair? '((split) . pea)) '#t)
(check-equal? (pair? '()) '#f)
(check-equal? (car '(pear)) 'pear)
(check-equal? (cdr '(pear)) '())
(check-equal? (cons '(split) 'pea) '((split) . pea))
(check-equal?
 (run* r (fresh (x y) (== (cons x (cons y 'salad)) r)))
 '((_0 _1 . salad)))
(check-equal? (run* q (pairo (cons q q))) '(_0))
(check-equal? (run* q (pairo '())) '())
(check-equal? (run* q (pairo 'pair)) '())
(check-equal? (run* x (pairo x)) '((_0 . _1)))
(check-equal? (run* r (pairo (cons r '()))) '(_0))
(check-equal? (singleton? '((a) (a b) c)) '#f)
(check-equal? (singleton? '()) '#f)
(check-equal? (singleton? (cons 'pea '())) '#t)
(check-equal? (singleton? '(sauerkraut)) '#t)
