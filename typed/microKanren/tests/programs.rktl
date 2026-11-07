;; Adapted from microKanren-test-programs.scm in
;; https://github.com/jasonhemann/microKanren
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.
;; MIT License; see NOTICE.

(: a-and-b Goal)
(define a-and-b
  (conj
   (call/fresh (λ (a) (== a 7)))
   (call/fresh
    (λ (b)
      (disj
       (== b 5)
       (== b 6))))))

(: fives (→ Term Goal))
(define (fives x)
  (disj
   (== x 5)
   (λ ([a/c : State])
     (λ ()
       ((fives x) a/c)))))

(: appendo (→ Term Term Term Goal))
(define (appendo l s out)
  (disj
   (conj (== '() l) (== s out))
   (call/fresh
    (λ (a)
      (call/fresh
       (λ (d)
         (conj
          (== `(,a . ,d) l)
          (call/fresh
           (λ (res)
             (conj
              (== `(,a . ,res) out)
              (λ ([s/c : State])
                (λ ()
                  ((appendo d s res) s/c)))))))))))))

(: appendo2 (→ Term Term Term Goal))
(define (appendo2 l s out)
  (disj
   (conj (== '() l) (== s out))
   (call/fresh
    (λ (a)
      (call/fresh
       (λ (d)
         (conj
          (== `(,a . ,d) l)
          (call/fresh
           (λ (res)
             (conj
              (λ ([s/c : State])
                (λ ()
                  ((appendo2 d s res) s/c)))
              (== `(,a . ,res) out)))))))))))

(: call-appendo Goal)
(define call-appendo
  (call/fresh
   (λ (q)
     (call/fresh
      (λ (l)
        (call/fresh
         (λ (s)
           (call/fresh
            (λ (out)
              (conj
               (appendo l s out)
               (== `(,l ,s ,out) q)))))))))))

(: call-appendo2 Goal)
(define call-appendo2
  (call/fresh
   (λ (q)
     (call/fresh
      (λ (l)
        (call/fresh
         (λ (s)
           (call/fresh
            (λ (out)
              (conj
               (appendo2 l s out)
               (== `(,l ,s ,out) q)))))))))))

(: call-appendo3 Goal)
(define call-appendo3
  (call/fresh
   (λ (q)
     (call/fresh
      (λ (l)
        (call/fresh
         (λ (s)
           (call/fresh
            (λ (out)
              (conj
               (== `(,l ,s ,out) q)
               (appendo l s out)))))))))))

(: ground-appendo Goal)
(define ground-appendo (appendo '(a) '(b) '(a b)))

(: ground-appendo2 Goal)
(define ground-appendo2 (appendo2 '(a) '(b) '(a b)))

(: relo (→ Term Goal))
(define (relo x)
  (call/fresh
   (λ (x1)
     (call/fresh
      (λ (x2)
        (conj
         (== x `(,x1 . ,x2))
         (disj
          (== x1 x2)
          (λ ([s/c : State])
            (λ () ((relo x) s/c))))))))))

(: many-non-ans Goal)
(define many-non-ans
  (call/fresh
   (λ (x)
     (disj
      (relo `(5 . 6))
      (== x 3)))))
