(displayln "Test code in chapter 8")
(: *o       (→ Term Term Term Goal))
(: odd-*o   (→ Term Term Term Term Goal))
(: bound-*o (→ Term Term Term Term Goal))
(define (*o n m p)
  (condi
    [(== '() n) (== '() p)]
    [(poso n) (== '() m) (== '() p)]
    [(== '(1) n) (poso m) (== m p)]
    [(>1o n) (== '(1) m) (== n p)]
    [(fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z))]
    [(fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p))]
    [(fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p))]
    [else fail]))
(define (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (+o `(0 . ,q) m p)))
(define (bound-*o q p n m)
  (conde
    [(nullo q) (pairo p)]
    [else
     (fresh (x y z)
       (cdro q x)
       (cdro p y)
       (condi
         [(nullo n)
          (cdro m z)
          (bound-*o x y z '())]
         [else
          (cdro n z)
          (bound-*o x y z m)]))]))
(check-equal?
 (run 34 (t)
   (fresh (x y r)
     (*o x y r)
     (== `(,x ,y ,r) t)))
 '((() _.0 ())
   ((_.0 . _.1) () ())
   ((1) (_.0 . _.1) (_.0 . _.1))
   ((_.0 _.1 . _.2) (1) (_.0 _.1 . _.2))
   ((0 1) (_.0 _.1 . _.2) (0 _.0 _.1 . _.2))
   ((1 _.0 . _.1) (0 1) (0 1 _.0 . _.1))
   ((0 0 1) (_.0 _.1 . _.2) (0 0 _.0 _.1 . _.2))
   ((1 1) (1 1) (1 0 0 1))
   ((0 1 _.0 . _.1) (0 1) (0 0 1 _.0 . _.1))
   ((1 _.0 . _.1) (0 0 1) (0 0 1 _.0 . _.1))
   ((0 0 0 1) (_.0 _.1 . _.2) (0 0 0 _.0 _.1 . _.2))
   ((1 1) (1 0 1) (1 1 1 1))
   ((0 1 1) (1 1) (0 1 0 0 1))
   ((1 1) (0 1 1) (0 1 0 0 1))
   ((0 0 1 _.0 . _.1) (0 1) (0 0 0 1 _.0 . _.1))
   ((1 1) (1 1 1) (1 0 1 0 1))
   ((0 1 _.0 . _.1) (0 0 1) (0 0 0 1 _.0 . _.1))
   ((1 _.0 . _.1) (0 0 0 1) (0 0 0 1 _.0 . _.1))
   ((0 0 0 0 1) (_.0 _.1 . _.2) (0 0 0 0 _.0 _.1 . _.2))
   ((1 0 1) (1 1) (1 1 1 1))
   ((0 1 1) (1 0 1) (0 1 1 1 1))
   ((1 0 1) (0 1 1) (0 1 1 1 1))
   ((0 0 1 1) (1 1) (0 0 1 0 0 1))
   ((1 1) (1 0 0 1) (1 1 0 1 1))
   ((0 1 1) (0 1 1) (0 0 1 0 0 1))
   ((1 1) (0 0 1 1) (0 0 1 0 0 1))
   ((0 0 0 1 _.0 . _.1) (0 1) (0 0 0 0 1 _.0 . _.1))
   ((1 1) (1 1 0 1) (1 0 0 0 0 1))
   ((0 1 1) (1 1 1) (0 1 0 1 0 1))
   ((1 1 1) (0 1 1) (0 1 0 1 0 1))
   ((0 0 1 _.0 . _.1) (0 0 1) (0 0 0 0 1 _.0 . _.1))
   ((1 1) (1 0 1 1) (1 1 1 0 0 1))
   ((0 1 _.0 . _.1) (0 0 0 1) (0 0 0 0 1 _.0 . _.1))
   ((1 _.0 . _.1) (0 0 0 0 1) (0 0 0 0 1 _.0 . _.1))))
(check-equal?
 (run* (p)
   (*o '(0 1) '(0 0 1) p))
 '((0 0 0 1)))
(check-equal?
 (run 1 (t)
   (fresh (n m)
     (*o n m '(1))
     (== `(,n ,m) t)))
 '(((1) (1))))
(check-equal?
 (run 2 (t)
   (fresh (n m)
     (*o n m '(1))
     (== `(,n ,m) t)))
 '(((1) (1))))
(check-equal?
 (run* (p)
   (*o '(1 1 1) '(1 1 1 1 1 1) p))
 '((1 0 0 1 1 1 0 1 1)))

(: =lo (→ Term Term Goal))
(define (=lo n m)
  (conde
    [(== '() n) (== '() m)]
    [(== '(1) n) (== '(1) m)]
    [else
     (fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y))]))
(check-equal?
 (run* (t)
   (fresh (w x y)
     (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))
     (== `(,w ,x ,y) t)))
 '((_.0 _.1 (_.2 1))))
(check-equal?
 (run* (b)
   (=lo '(1) `(,b)))
 '(1))
(check-equal?
 (run* (n)
   (=lo `(1 0 1 . ,n) '(0 1 1 0 1)))
 '((_.0 1)))
(check-equal?
 (run 5 (t)
   (fresh (y z)
     (=lo `(1 . ,y) `(1 . ,z))
     (== `(,y ,z) t)))
 '((() ())
   ((1) (1))
   ((_.0 1) (_.1 1))
   ((_.0 _.1 1) (_.2 _.3 1))
   ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))))
(check-equal?
 (run 5 (t)
   (fresh (y z)
     (=lo `(1 . ,y) `(0 . ,z))
     (== `(,y ,z) t)))
 '(((1) (1))
   ((_.0 1) (_.1 1))
   ((_.0 _.1 1) (_.2 _.3 1))
   ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
   ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1))))
(check-equal?
 (run 5 (t)
   (fresh (y z)
     (=lo `(1 . ,y) `(0 1 1 0 1 . ,z))
     (== `(,y ,z) t)))
 '(((_.0 _.1 _.2 1) ())
   ((_.0 _.1 _.2 _.3 1) (1))
   ((_.0 _.1 _.2 _.3 _.4 1) (_.5 1))
   ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 1))
   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 1) (_.7 _.8 _.9 1))))

(: <lo (→ Term Term Goal))
(define (<lo n m)
  (conde
    [(== '() n) (poso m)]
    [(== '(1) n) (>1o m)]
    [else
     (fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y))]))
(check-equal?
 (run 8 (t)
   (fresh (y z)
     (<lo `(1 . ,y) `(0 1 1 0 1 . ,z))
     (== `(,y ,z) t)))
 '((() _.0)
   ((1) _.0)
   ((_.0 1) _.1)
   ((_.0 _.1 1) _.2)
   ((_.0 _.1 _.2 1) (_.3 . _.4))
   ((_.0 _.1 _.2 _.3 1) (_.4 _.5 . _.6))
   ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 . _.8))
   ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 . _.10))))

(: <=lo (→ Term Term Goal))
(define (<=lo n m)
  (condi
    [(=lo n m) succeed]
    [(<lo n m) succeed]
    [else fail]))
(check-equal?
 (run 10 (t)
   (fresh (n m)
     (<=lo n m)
     (*o n '(0 1) m)
     (== `(,n ,m) t)))
 '((() ())
   ((1) (0 1))
   ((0 1) (0 0 1))
   ((1 1) (0 1 1))
   ((0 0 1) (0 0 0 1))
   ((1 _.0 1) (0 1 _.0 1))
   ((0 1 1) (0 0 1 1))
   ((0 0 0 1) (0 0 0 0 1))
   ((1 _.0 _.1 1) (0 1 _.0 _.1 1))
   ((0 1 _.0 1) (0 0 1 _.0 1))))
(check-equal?
 (run 15 (t)
   (fresh (n m)
     (<=lo n m)
     (== `(,n ,m) t)))
 '((() ())
   (() (_.0 . _.1))
   ((1) (1))
   ((1) (_.0 _.1 . _.2))
   ((_.0 1) (_.1 1))
   ((_.0 1) (_.1 _.2 _.3 . _.4))
   ((_.0 _.1 1) (_.2 _.3 1))
   ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6))
   ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
   ((_.0 _.1 _.2 1) (_.3 _.4 _.5 _.6 _.7 . _.8))
   ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1))
   ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 _.8 _.9 . _.10))
   ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 _.8 _.9 1))
   ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 . _.12))
   ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 _.10 _.11 1))))

(: <o (→ Term Term Goal))
(define (<o n m)
  (condi
    [(<lo n m) succeed]
    [(=lo n m)
     (fresh (x)
       (poso x)
       (+o n x m))]
    [else fail]))
(: <=o (→ Term Term Goal))
(define (<=o n m)
  (condi
    [(== n m) succeed]
    [(<o n m) succeed]
    [else fail]))
(check-equal?
 (run* (q)
   (<o '(1 0 1) '(1 1 1))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (<o '(1 1 1) '(1 0 1))
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (<o '(1 0 1) '(1 0 1))
   (== #t q))
 '())
(check-equal?
 (run 6 (n)
   (<o n '(1 0 1)))
 '(() (0 0 1) (1) (_.0 1)))
(check-equal?
 (run 6 (m)
   (<o '(1 0 1) m))
 '((_.0 _.1 _.2 _.3 . _.4) (0 1 1) (1 1 1)))

(: splito (→ Term Term Term Term Goal))
(define (splito n r l h)
  (condi
    [(== '() n) (== '() h) (== '() l)]
    [(fresh (b n^)
       (== `(0 ,b . ,n^) n)
       (== '() r)
       (== `(,b . ,n^) h)
       (== '() l))]
    [(fresh (n^)
       (== `(1 . ,n^) n)
       (== '() r)
       (== n^ h)
       (== '(1) l))]
    [(fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== '() l)
       (splito `(,b . ,n^) r^ '() h))]
    [(fresh (n^ a r^)
       (== `(1 . ,n^) n)
       (== `(,a . ,r^) r)
       (== '(1) l)
       (splito n^ r^ '() h))]
    [(fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)
       (== `(,a . ,r^) r)
       (== `(,b . ,l^) l)
       (poso l^)
       (splito n^ r^ l^ h))]
    [else fail]))
(: ÷o (→ Term Term Term Term Goal))
(define (÷o n m q r)
  (condi
    [(== r n) (== '() q) (<o n m)]
    [(== '(1) q) (=lo n m) (+o r m n)
     (<o r m)]
    [else
     (alli
       (<lo m n)
       (<o r m)
       (poso q)
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (alli
           (splito n r nl nh)
           (splito q r ql qh)
           (conde
             [(== '() nh)
              (== '() qh)
              (-o nl r qlm)
              (*o ql m qlm)]
             [else
              (alli
                (poso nh)
                (*o ql m qlm)
                (+o qlm r qlmr)
                (-o qlmr nl rr)
                (splito rr r '() rh)
                (÷o nh m qh rh))]))))]))
(check-equal?
 (run 15 (t)
   (fresh (n m q r)
     (÷o n m q r)
     (== `(,n ,m ,q ,r) t)))
 '((() (_.0 . _.1) () ())
   ((1) (1) (1) ())
   ((0 1) (1 1) () (0 1))
   ((0 1) (1) (0 1) ())
   ((1) (_.0 _.1 . _.2) () (1))
   ((_.0 1) (_.0 1) (1) ())
   ((0 _.0 1) (1 _.0 1) () (0 _.0 1))
   ((0 _.0 1) (_.0 1) (0 1) ())
   ((_.0 1) (_.1 _.2 _.3 . _.4) () (_.0 1))
   ((1 1) (0 1) (1) (1))
   ((0 0 1) (0 1 1) () (0 0 1))
   ((1 1) (1) (1 1) ())
   ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6) () (_.0 _.1 1))
   ((_.0 _.1 1) (_.0 _.1 1) (1) ())
   ((1 0 1) (0 1 1) () (1 0 1))))
(check-equal?
 (run* (m)
   (fresh (r)
     (÷o '(1 0 1) m '(1 1 1) r)))
 '())
(check-equal?
 (run 3 (t)
   (fresh (y z)
     (÷o `(1 0 . ,y) '(0 1) z '())
     (== `(,y ,z) t)))
 '())

(: exp2o (→ Term Term Term Goal))
(define (exp2o n b q)
  (condi
    [(== '(1) n) (== '() q)]
    [(>1o n) (== '(1) q)
     (fresh (s)
       (splito n b s '(1)))]
    [(fresh (q1 b2)
       (alli
         (== `(0 . ,q1) q)
         (poso q1)
         (<lo b n)
         (appendo b `(1 . ,b) b2)
         (exp2o n b2 q1)))]
    [(fresh (q1 nh b2 s)
       (alli
         (== `(1 . ,q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b `(1 . ,b) b2)
         (exp2o nh b2 q1)))]
    [else fail]))
(: repeated-mulo (→ Term Term Term Goal))
(define (repeated-mulo n q nq)
  (conde
    [(poso n) (== '() q) (== '(1) nq)]
    [(== '(1) q) (== n nq)]
    [(>1o q)
     (fresh (q1 nq1)
       (+o q1 '(1) q)
       (repeated-mulo n q1 nq1)
       (*o nq1 n nq))]
    [else fail]))
(: logo (→ Term Term Term Term Goal))
(define (logo n b q r)
  (condi
    [(== '(1) n) (poso b) (== '() q) (== '() r)]
    [(== '() q) (<o n b) (+o r '(1) n)]
    [(== '(1) q) (>1o b) (=lo n b) (+o r b n)]
    [(== '(1) b) (poso q) (+o r '(1) n)]
    [(== '() b) (poso q) (== r n)]
    [(== '(0 1) b)
     (fresh (a ad dd)
       (poso dd)
       (== `(,a ,ad . ,dd) n)
       (exp2o n '() q)
       (fresh (s)
         (splito n dd r s)))]
    [(fresh (a ad add ddd)
       (conde
         [(== '(1 1) b)]
         [else (== `(,a ,ad ,add . ,ddd) b)]))
     (<lo b n)
     (fresh (bw1 bw nw nw1 ql1 ql s)
       (exp2o b '() bw1)
       (+o bw1 '(1) bw)
       (<lo q n)
       (fresh (q1 bwq1)
         (+o q '(1) q1)
         (*o bw q1 bwq1)
         (<o nw1 bwq1)
         (exp2o n '() nw1)
         (+o nw1 '(1) nw)
         (÷o nw bw ql1 s)
         (+o ql '(1) ql1)
         (conde
           [(== q ql)]
           [else (<lo ql q)])
         (fresh (bql qh s qdh qd)
           (repeated-mulo b ql bql)
           (÷o nw bw1 qh s)
           (+o ql qdh qh)
           (+o ql qd q)
           (conde
             [(== qd qdh)]
             [else (<o qd qdh)])
           (fresh (bqd bq1 bq)
             (repeated-mulo b qd bqd)
             (*o bql bqd bq)
             (*o b bq bq1)
             (+o bq r n)
             (<o n bq1)))))]
    [else fail]))
(check-equal?
 (run* (r)
   (logo '(0 1 1 1) '(0 1) '(1 1) r))
 '((0 1 1)))
(check-equal?
 (run 8 (s)
   (fresh (b q r)
     (logo '(0 0 1 0 0 0 1) b q r)
     (>1o q)
     (== `(,b ,q ,r) s)))
 '(((1) (_.0 _.1 . _.2) (1 1 0 0 0 0 1))
   (() (_.0 _.1 . _.2) (0 0 1 0 0 0 1))
   ((0 1) (0 1 1) (0 0 1))
   ((0 0 1) (1 1) (0 0 1))
   ((1 0 1) (0 1) (1 1 0 1 0 1))
   ((0 1 1) (0 1) (0 0 0 0 0 1))
   ((1 1 1) (0 1) (1 1 0 0 1))
   ((0 0 0 1) (0 1) (0 0 1))))

(: expo (→ Term Term Term Goal))
(define (expo b q n)
  (logo n b q '()))
(check-equal?
 (run* (t)
   (expo '(1 1) '(1 0 1) t))
 '((1 1 0 0 1 1 1 1)))
