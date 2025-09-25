(displayln "Test code in chapter 7")
(: bit-xoro (→ Term Term Term Goal))
(define (bit-xoro x y r)
  (conde
    [(== 0 x) (== 0 y) (== 0 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]
    [else fail]))
(: bit-nando (→ Term Term Term Goal))
(define (bit-nando x y r)
  (conde
    [(== 0 x) (== 0 y) (== 1 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]
    [else fail]))
(check-equal?
 (run* (s)
   (fresh (x y)
     (bit-xoro x y 0)
     (== `(,x ,y) s)))
 '((0 0) (1 1)))
(check-equal?
 (run* (s)
   (fresh (x y)
     (bit-xoro x y 1)
     (== `(,x ,y) s)))
 '((1 0) (0 1)))
(check-equal?
 (run* (s)
   (fresh (x y r)
     (bit-xoro x y r)
     (== `(,x ,y ,r) s)))
 '((0 0 0)
   (1 0 1)
   (0 1 1)
   (1 1 0)))

(: bit-ando (→ Term Term Term Goal))
(define (bit-ando x y r)
  (conde
    [(== 0 x) (== 0 y) (== 0 r)]
    [(== 1 x) (== 0 y) (== 0 r)]
    [(== 0 x) (== 1 y) (== 0 r)]
    [(== 1 x) (== 1 y) (== 1 r)]
    [else fail]))
(: bit-noto (→ Term Term Goal))
(define (bit-noto x r)
  (bit-nando x x r))
(check-equal?
 (run* (s)
   (fresh (x y)
     (bit-ando x y 1)
     (== `(,x ,y) s)))
 '((1 1)))

(: half-addero (→ Term Term Term Term Goal))
(define (half-addero x y r c)
  (all (bit-xoro x y r)
       (bit-ando x y c)))
(check-equal?
 (run* (r)
   (half-addero 1 1 r 1))
 '(0))
(check-equal?
 (run* (s)
   (fresh (x y r c)
     (half-addero x y r c)
     (== `(,x ,y ,r ,c) s)))
 '((0 0 0 0)
   (1 0 1 0)
   (0 1 1 0)
   (1 1 0 1)))

(: full-addero (→ Term Term Term Term Term Goal))
(define (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))
(check-equal?
 (run* (s)
   (fresh (r c)
     (full-addero 0 1 1 r c)
     (== `(,r ,c) s)))
 '((0 1)))
(check-equal?
 (run* (s)
   (fresh (r c)
     (full-addero 1 1 1 r c)
     (== `(,r ,c) s)))
 '((1 1)))
(check-equal?
 (run* (s)
   (fresh (b x y r c)
     (full-addero b x y r c)
     (== `(,b ,x ,y ,r ,c) s)))
 '((0 0 0 0 0)
   (1 0 0 1 0)
   (0 1 0 1 0)
   (1 1 0 0 1)
   (0 0 1 1 0)
   (1 0 1 0 1)
   (0 1 1 0 1)
   (1 1 1 1 1)))

(define-type Bit (∪ Zero One))
(define-predicate bit? Bit)
(: build-num (→ Natural (Listof Bit)))
(define (build-num n)
  (if (zero? n)
      '()
      (let-values ([(n m) (quotient/remainder n 2)])
        (cons (assert m bit?) (build-num n)))))

(: poso (→ Term Goal))
(define (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))
(check-equal?
 (run* (q)
   (poso '(0 1 1))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (poso '(1))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (poso '())
   (== #t q))
 '())
(check-equal?
 (run* (r)
   (poso r))
 '((_.0 . _.1)))

(: >1o (→ Term Goal))
(define (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))
(check-equal?
 (run* (q)
   (>1o '(0 1 1))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (>1o '(0 1))
   (== #t q))
 '(#t))
(check-equal?
 (run* (q)
   (>1o '(1))
   (== #t q))
 '())
(check-equal?
 (run* (q)
   (>1o '())
   (== #t q))
 '())
(check-equal?
 (run* (r)
   (>1o r))
 '((_.0 _.1 . _.2)))

(: addero     (→ Term Term Term Term Goal))
(: gen-addero (→ Term Term Term Term Goal))
(define (addero d n m r)                ; r = d + n + m, d = 0 or 1
  (condi
    [(== 0 d) (== '() m) (== n r)]      ; d = 0, m = 0, r = n
    [(== 0 d) (== '() n) (== m r)       ; d = 0, n = 0, r = m
     (poso m)]                          ; m > 0
    [(== 1 d) (== '() m)                ; d = 1, m = 0
     (addero 0 n '(1) r)]               ; r = n + 1
    [(== 1 d) (== '() n) (poso m)       ; d = 1, n = 0, m > 0
     (addero 0 '(1) m r)]               ; r = 1 + m
    [(== '(1) n) (== '(1) m)            ; n = 1, m = 1
     (fresh (a c)
       (== `(,a ,c) r)                  ; r = 2*c + a
       (full-addero d 1 1 a c))]        ; 2*c + a = d + 1 + 1
    [(== '(1) n) (gen-addero d n m r)]  ; n = 1
    [(== '(1) m) (>1o n) (>1o r)        ; m = 1, n > 1, r > 1
     (addero d '(1) n r)]               ; r = d + 1 + n
    [(>1o n) (gen-addero d n m r)]      ; n > 1
    [else fail]))
(define (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)                   ; n = 2*x + a
    (== `(,b . ,y) m) (poso y)          ; m = 2*y + b, y > 0
    (== `(,c . ,z) r) (poso z)          ; r = 2*z + c, z > 0
    (alli
      (full-addero d a b c e)           ; 2*e + c = d + a + b
      (addero e x y z))))               ; z = e + x + y
(: width (→ Term Natural))
(define (width n)
  (cond
    [(null? n) 0]
    [(pair? n) (add1 (width (cdr n)))]
    [else 1]))
(check-equal?
 (run 3 (s)
   (fresh (x y r)
     (addero 0 x y r)
     (== `(,x ,y ,r) s)))
 '((_.0 () _.0)
   (() (_.0 . _.1) (_.0 . _.1))
   ((1) (1) (0 1))))
(check-equal?
 (run 22 (s)
   (fresh (x y r)
     (addero 0 x y r)
     (== `(,x ,y ,r) s)))
 '((_.0 () _.0)
   (() (_.0 . _.1) (_.0 . _.1))
   ((1) (1) (0 1))
   ((1) (0 _.0 . _.1) (1 _.0 . _.1))
   ((0 _.0 . _.1) (1) (1 _.0 . _.1))
   ((1) (1 1) (0 0 1))
   ((0 1) (0 1) (0 0 1))
   ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))
   ((1 1) (1) (0 0 1))
   ((1) (1 1 1) (0 0 0 1))
   ((1 1) (0 1) (1 0 1))
   ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
   ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
   ((1) (1 1 1 1) (0 0 0 0 1))
   ((0 1) (0 0 _.0 . _.1) (0 1 _.0 . _.1))
   ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
   ((1 1 1) (1) (0 0 0 1))
   ((1) (1 1 1 1 1) (0 0 0 0 0 1))
   ((0 1) (1 1) (1 0 1))
   ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
   ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
   ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))))
(check-equal?
 (run* (s)
   (gen-addero 1 '(0 1 1) '(1 1) s))
 '((0 1 0 1)))
(check-equal?
 (run* (s)
   (fresh (x y)
     (addero 0 x y '(1 0 1))
     (== `(,x ,y) s)))
 '(((1 0 1) ())
   (() (1 0 1))
   ((1) (0 0 1))
   ((0 0 1) (1))
   ((1 1) (0 1))
   ((0 1) (1 1))))

(: +o (→ Term Term Term Goal))
(define (+o n m k)
  (addero 0 n m k))
(check-equal?
 (run* (s)
   (fresh (x y)
     (+o x y '(1 0 1))
     (== `(,x ,y) s)))
 '(((1 0 1) ())
   (() (1 0 1))
   ((1) (0 0 1))
   ((0 0 1) (1))
   ((1 1) (0 1))
   ((0 1) (1 1))))

(: -o (→ Term Term Term Goal))
(define (-o n m k)
  (+o m k n))
(check-equal?
 (run* (q)
   (-o '(0 0 0 1) '(1 0 1) q))
 '((1 1)))
(check-equal?
 (run* (q)
   (-o '(0 1 1) '(0 1 1) q))
 '(()))
(check-equal?
 (run* (q)
   (-o '(0 1 1) '(0 0 0 1) q))
 '())
