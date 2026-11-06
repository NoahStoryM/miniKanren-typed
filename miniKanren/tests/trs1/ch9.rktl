(displayln "Test code in chapter 9")
(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))
(define lhs car)
(define rhs cdr)

(check-equal? (rhs `(,z . b)) 'b)
(check-equal? (rhs `(,z . ,w)) w)
(check-equal? (rhs `(,z . (,x e ,y))) `(,x e ,y))

(check-equal? (walk z `([,z . a] [,x . ,w] [,y . ,z])) 'a)
(check-equal? (walk y `([,z . a] [,x . ,w] [,y . ,z])) 'a)
(check-equal? (walk x `([,z . a] [,x . ,w] [,y . ,z])) w)
(check-equal? (walk w `([,z . a] [,x . ,w] [,y . ,z])) w)
(check-equal? (walk w `([,x . ,y] [,w . b] [,z . ,x] [,y . ,z])) 'b)
(check-equal? (walk x `([,y . b] [,x . ,y] [,v . ,x] [,w . ,x] [,u . ,w])) 'b)
(check-equal? (walk x `([,y . ,z] [,x . ,y] [,v . ,x] [,w . ,x] [,u . ,w])) z)
(check-equal? (walk u `([,y . ,z] [,x . ,y] [,v . ,x] [,w . ,x] [,u . ,w])) z)
(check-equal? (walk v `([,y . ,z] [,x . ,y] [,v . ,x] [,w . ,x] [,u . ,w])) z)
(check-equal? (walk w `([,y . ,z] [,x . ,y] [,v . ,x] [,w . ,x] [,u . ,w])) z)
(check-equal? (walk u `([,x . b] [,w . (,x e ,x)] [,u . ,w])) `(,x e ,x))
(check-equal? (walk y `([,x . e])) y)
#;(check-equal? (walk y (ext-s y x `([,x . e]))) 'e)
(check-equal? (walk x `([,y . ,z] [,x . ,y])) z)
(check-equal? (walk x (ext-s z 'b `([,y . ,z] [,x . ,y]))) 'b)
#;(check-equal? (walk x (ext-s z w `([,y . ,z] [,x . ,y]))) w)

(check-equal? (walk* x `([,y . (a ,z c)] [,x . ,y] [,z . a])) '(a a c))
(check-equal? (walk* x `([,y . (,z ,w c)] [,x . ,y] [,z . a])) `(a ,w c))
(check-equal? (walk* y `([,y . (,w ,z c)] [,v . b] [,x . ,v] [,z . ,x])) `(,w b c))

(define-syntax project
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (λG (s) (let ([x (walk* x s)] ...) ((all g ...) s)))]))
(check-equal?
 (run* (q)
   (== #f q)
   (project (q)
     (== (not (not q)) q)))
 '(#f))

(check-equal?
 (let ([r `(,w ,x ,y)])
   (walk* r (reify-s r empty-s)))
 '(_.0 _.1 _.2))
(check-equal?
 (let ([r (walk* `(,x ,y ,z) empty-s)])
   (walk* r (reify-s r empty-s)))
 '(_.0 _.1 _.2))
(check-equal?
 (let ([r `(,u (,v (,w ,x) ,y) ,x)])
   (walk* r (reify-s r empty-s)))
 '(_.0 (_.1 (_.2 _.3) _.4) _.3))
(check-equal?
 (let ([s `([,y . (,z ,w c ,w)] [,x . ,y] [,z . a])])
   (let ([r (walk* x s)])
     (walk* r (reify-s r empty-s))))
 '(a _.0 c _.0))
(check-equal?
 (let ([s `([,y . (,z ,w c ,w)] [,x . ,y] [,z . a])])
   (reify (walk* x s)))
 '(a _.0 c _.0))

#;
(check-equal?
 (run 1 (q)
   (fresh (x)
     (== `(,x) x)
     (== #t q)))
 '())
#;
(check-equal?
 (run 1 (q)
   (fresh (x y)
     (== `(,x) y)
     (== `(,y) x)
     (== #t q)))
 '())
#;
(check-equal?
 (run 1 (x)
   (== `(,x) x))
 '())
#;
(check-equal?
 (run 1 (x)
   (fresh (y z)
     (== x z)
     (== `(a b ,z) y)
     (== x y)))
 '())
