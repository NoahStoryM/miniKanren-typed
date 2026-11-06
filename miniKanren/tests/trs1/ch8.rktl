(displayln "Test code in chapter 8")
(: *o       (→ Term Term Term Goal))
(: odd-*o   (→ Term Term Term Term Goal))
(: bound-*o (→ Term Term Term Term Goal))
(define (*o n m p)                      ; p = n*m
  (condi
    [(== '() n) (== '() p)]             ; n = 0, p = 0
    [(poso n) (== '() m) (== '() p)]    ; n > 0, m = 0, p = 0
    [(== '(1) n) (poso m) (== m p)]     ; n = 1, m > 0, p = m
    [(>1o n) (== '(1) m) (== n p)]      ; n > 1, m = 1, p = n
    [(fresh (x z)
       (== `(0 . ,x) n) (poso x)        ; n = 2*x, x > 0
       (== `(0 . ,z) p) (poso z)        ; p = 2*z, z > 0
       (>1o m)                          ; m > 1
       (*o x m z))]                     ; z = x*m
    [(fresh (x y)
       (== `(1 . ,x) n) (poso x)        ; n = 2*x + 1, x > 0
       (== `(0 . ,y) m) (poso y)        ; m = 2*y, y > 0
       (*o m n p))]                     ; p = m*n
    [(fresh (x y)
       (== `(1 . ,x) n) (poso x)        ; n = 2*x + 1, x > 0
       (== `(1 . ,y) m) (poso y)        ; m = 2*y + 1, y > 0
       (odd-*o x n m p))]
    [else fail]))
(define (odd-*o x n m p)
  ;; p = n*m
  ;;   = (2*x + 1)*m
  ;;   = 2*x*m + m (distributive law)
  ;;   = 2*q + m
  (fresh (q)
    (bound-*o q p n m)                  ; l(p) ≤ l(n) + l(m)
    (*o x m q)                          ; q = x*m
    (+o `(0 . ,q) m p)))                ; p = 2*q + m
(define (bound-*o q p n m)              ; l(p) ≤ l(n) + l(m)
  (conde
    [(nullo q) (pairo p)]               ; l(q) = 0, l(p) > 0
    [else
     (fresh (x y z)
       (cdro q x)                       ; l(x) + 1 = l(q)
       (cdro p y)                       ; l(y) + 1 = l(p)
       (condi
         [(nullo n)                     ; n = 0
          (cdro m z)                    ; l(z) + 1 = l(m)
          (bound-*o x y z '())]         ; l(y) ≤ l(z)
         [else
          (cdro n z)                    ; l(z) + 1 = l(n)
          (bound-*o x y z m)]))]))      ; l(y) ≤ l(z) + l(m)
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
(define (=lo n m)                       ; l(n) = l(m)
  (conde
    [(== '() n) (== '() m)]             ; n = 0, m = 0
    [(== '(1) n) (== '(1) m)]           ; n = 1, m = 1
    [else
     (fresh (a x b y)
       (== `(,a . ,x) n) (poso x)       ; n = 2*x + a, x > 0
       (== `(,b . ,y) m) (poso y)       ; m = 2*y + b, y > 0
       (=lo x y))]))                    ; l(x) = l(y)
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
(define (<lo n m)                       ; l(n) < l(m)
  (conde
    [(== '() n) (poso m)]               ; n = 0, m > 0
    [(== '(1) n) (>1o m)]               ; n = 1, m > 1
    [else
     (fresh (a x b y)
       (== `(,a . ,x) n) (poso x)       ; n = 2*x + a, x > 0
       (== `(,b . ,y) m) (poso y)       ; m = 2*y + b, y > 0
       (<lo x y))]))                    ; l(x) < l(y)
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
(define (<=lo n m)                      ; l(n) ≤ l(m)
  (condi
    [(=lo n m) succeed]                 ; l(n) = l(m)
    [(<lo n m) succeed]                 ; l(n) < l(m)
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
(define (<o n m)                        ; n < m
  (condi
    [(<lo n m) succeed]                 ; l(n) < l(m)
    [(=lo n m)                          ; l(n) = l(m)
     (fresh (x)
       (poso x)                         ; x > 0
       (+o n x m))]                     ; m = n + x
    [else fail]))
(: <=o (→ Term Term Goal))
(define (<=o n m)                       ; n ≤ m
  (condi
    [(== n m) succeed]                  ; n = m
    [(<o n m) succeed]                  ; n < m
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
(define (splito n r l h)                ; n = h*2^(l(r) + 1) + l
  (condi
    [(== '() n) (== '() h) (== '() l)]  ; n = 0, h = 0, l = 0
    [(fresh (b n^)
       (== `(0 ,b . ,n^) n)             ; n = 2*(2*n^ + b)
       (== '() r)                       ; r = 0
       (== `(,b . ,n^) h)               ; h = 2*n^ + b
       (== '() l))]                     ; l = 0
    [(fresh (n^)
       (== `(1 . ,n^) n)                ; n = 2*n^ + 1
       (== '() r)                       ; r = 0
       (== n^ h)                        ; h = n^
       (== '(1) l))]                    ; l = 1
    [(fresh (b n^ a r^)
       (== `(0 ,b . ,n^) n)             ; n = 2*(2*n^ + b)
       (== `(,a . ,r^) r)               ; r = 2*r^ + a
       (== '() l)                       ; l = 0
       (splito `(,b . ,n^) r^ '() h))]  ; 2*n^ + b = h*2^(l(r^) + 1)
    [(fresh (n^ a r^)
       (== `(1 . ,n^) n)                ; n = 2*n^ + 1
       (== `(,a . ,r^) r)               ; r = 2*r^ + a
       (== '(1) l)                      ; l = 1
       (splito n^ r^ '() h))]           ; n^ = h*2^(l(r^) + 1)
    [(fresh (b n^ a r^ l^)
       (== `(,b . ,n^) n)               ; n = 2*n^ + b
       (== `(,a . ,r^) r)               ; r = 2*r^ + a
       (== `(,b . ,l^) l)               ; l = 2*l^ + b
       (poso l^)                        ; l^ > 0
       (splito n^ r^ l^ h))]            ; n^ = h*2^(l(r^) + 1) + l^
    [else fail]))
(: ÷o (→ Term Term Term Term Goal))
(define (÷o n m q r)                    ; n = m*q + r, r < m
  (condi
    [(== r n) (== '() q) (<o n m)]      ; n = r, q = 0, n < m
    [(== '(1) q) (=lo n m) (+o r m n)   ; q = 1, l(n) = l(m), n = r + m
     (<o r m)]                          ; r < m
    [else
     (alli
       (<lo m n)                        ; l(m) < l(n)
       (<o r m)                         ; r < m
       (poso q)                         ; q > 0
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (alli
           (splito n r nl nh)           ; n = nh*2^(l(r) + 1) + nl
           (splito q r ql qh)           ; q = qh*2^(l(r) + 1) + ql
           (conde
             [(== '() nh)               ; nh = 0
              (== '() qh)               ; qh = 0
              (-o nl r qlm)             ; qlm = nl - r
              (*o ql m qlm)]            ; qlm = ql * m
             [else
              (alli
                (poso nh)               ; nh > 0
                (*o ql m qlm)           ; qlm = ql * m
                (+o qlm r qlmr)         ; qlmr = qlm + r
                (-o qlmr nl rr)         ; rr = qlmr - nl
                (splito rr r '() rh)    ; rr = rh*2^(l(r) + 1)
                (÷o nh m qh rh))]))))])); nh = m*qh + rh
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
(define (exp2o n b q)                   ; l(n) = q + l(b) + 1
  (condi
    [(== '(1) n) (== '() q)]            ; n = 1, q = 0
    [(>1o n) (== '(1) q)                ; n > 1, q = 1
     (fresh (s)
       (splito n b s '(1)))]            ; n = 2^(l(b) + 1) + s
    [(fresh (q1 b2)
       (alli
         (== `(0 . ,q1) q)              ; q = 2*q1
         (poso q1)                      ; q1 > 0
         (<lo b n)                      ; l(b) < l(n)
         (appendo b `(1 . ,b) b2)       ; l(b2) + 1 = 2*(l(b) + 1)
         (exp2o n b2 q1)))]             ; l(n) = q1 + l(b2) + 1
    [(fresh (q1 nh b2 s)
       (alli
         (== `(1 . ,q1) q)              ; q = 2*q1 + 1
         (poso q1)                      ; q1 > 0
         (poso nh)                      ; nh > 0
         (splito n b s nh)              ; n = nh*2^(l(b) + 1) + s
         (appendo b `(1 . ,b) b2)       ; l(b2) + 1 = 2*(l(b) + 1)
         (exp2o nh b2 q1)))]            ; l(nh) = q1 + l(b2) + 1
    [else fail]))
(: repeated-mulo (→ Term Term Term Goal))
(define (repeated-mulo n q nq)          ; nq = n * q
  (conde
    [(poso n) (== '() q) (== '(1) nq)]  ; n > 0, q = 0, nq = 1
    [(== '(1) q) (== n nq)]             ; q = 1, nq = n
    [(>1o q)                            ; q > 1
     (fresh (q1 nq1)
       (+o q1 '(1) q)                   ; q = q1 + 1
       (repeated-mulo n q1 nq1)         ; nq1 = n * q1
       (*o nq1 n nq))]                  ; nq = nq1 * n
    [else fail]))
(: logo (→ Term Term Term Term Goal))
(define (logo n b q r)                           ; n = b^q + r
  (condi
    [(== '(1) n) (poso b) (== '() q) (== '() r)] ; n = 1, b > 0, q = 0, r = 0
    [(== '() q) (<o n b) (+o r '(1) n)]          ; q = 0, n < b, n = 1 + r
    [(== '(1) q) (>1o b) (=lo n b) (+o r b n)]   ; q = 1, b > 1, l(n) = l(b), n = b + r
    [(== '(1) b) (poso q) (+o r '(1) n)]         ; b = 1, q > 0, n = 1 + r
    [(== '() b) (poso q) (== r n)]               ; b = 0, q > 0, n = r
    [(== '(0 1) b)                               ; b = 2
     (fresh (a ad dd)
       (poso dd)                                 ; dd > 0
       (== `(,a ,ad . ,dd) n)                    ; n = a + 2*ad + 4*dd
       (exp2o n '() q)                           ; l(n) = q + 1
       (fresh (s)
         (splito n dd r s)))]                    ; n = s*2^(l(dd) + 1) + r
    [(fresh (a ad add ddd)
       (conde
         [(== '(1 1) b)]                         ; b = 3
         [else (== `(,a ,ad ,add . ,ddd) b)]))   ; b = a + 2*ad + 4*add + 8*ddd
     (<lo b n)                                   ; l(b) < l(n)
     (fresh (bw1 bw nw nw1 ql1 ql s)
       (exp2o b '() bw1)                         ; l(b) = bw1 + 1
       (+o bw1 '(1) bw)                          ; bw = bw1 + 1
       (<lo q n)                                 ; l(q) < l(n)
       (fresh (q1 bwq1)
         (+o q '(1) q1)                          ; q = q1 + 1
         (*o bw q1 bwq1)                         ; bwq1 = bw*q1
         (<o nw1 bwq1)                           ; nw1 < bwq1
         (exp2o n '() nw1)                       ; l(n) = nw1 + 1
         (+o nw1 '(1) nw)                        ; nw = nw1 + 1
         (÷o nw bw ql1 s)                        ; nw = bw*ql1 + s, s < bw
         (+o ql '(1) ql1)                        ; ql1 = ql + 1
         (conde
           [(== q ql)]                           ; q = ql
           [else (<lo ql q)])                    ; l(ql) < q
         (fresh (bql qh s qdh qd)
           (repeated-mulo b ql bql)              ; bql = b*ql
           (÷o nw bw1 qh s)                      ; nw = bw1*qh + s, s < bw1
           (+o ql qdh qh)                        ; qh = ql + qdh
           (+o ql qd q)                          ; q = ql + qd
           (conde
             [(== qd qdh)]                       ; qd = qdh
             [else (<o qd qdh)])                 ; qd < qdh
           (fresh (bqd bq1 bq)
             (repeated-mulo b qd bqd)            ; bqd = b*qd
             (*o bql bqd bq)                     ; bq = bql*bqd
             (*o b bq bq1)                       ; bq1 = b*bq
             (+o bq r n)                         ; n = bq + r
             (<o n bq1)))))]                     ; n < bq1
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
(define (expo b q n)                    ; n = b^q
  (logo n b q '()))
(check-equal?
 (run* (t)
   (expo '(1 1) '(1 0 1) t))
 '((1 1 0 0 1 1 1 1)))
