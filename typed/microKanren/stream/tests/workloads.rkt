#lang racket/base

;; Recursive programs shared by the original and both abstract backends.
;; Adapted microKanren examples; see the repository NOTICE.
(provide workloads)

(define (workloads == call/fresh disj conj delay-goal)
  (define-syntax-rule (Zzz goal)
    (delay-goal (λ () goal)))
  (define-syntax conj+
    (syntax-rules ()
      [(_ goal) (Zzz goal)]
      [(_ first rest ...) (conj (Zzz first) (conj+ rest ...))]))
  (define-syntax disj+
    (syntax-rules ()
      [(_ goal) (Zzz goal)]
      [(_ first rest ...) (disj (Zzz first) (disj+ rest ...))]))
  (define-syntax fresh
    (syntax-rules ()
      [(_ () first rest ...) (conj+ first rest ...)]
      [(_ (x rest ...) goal ...)
       (call/fresh (λ (x) (fresh (rest ...) goal ...)))]))
  (define-syntax-rule (conde [first rest ...] ...)
    (disj+ (conj+ first rest ...) ...))

  (define (never state) ((delay-goal (λ () never)) state))
  (define (fives q)
    (disj (== q 5) (Zzz (fives q))))
  (define (appendo left right out)
    (conde
      [(== '() left) (== right out)]
      [(fresh (head tail result)
         (== `(,head . ,tail) left)
         (== `(,head . ,result) out)
         (appendo tail right result))]))
  (define (rember*o tree out)
    (conde
      [(== '() tree) (== '() out)]
      [(fresh (head tail)
         (== `(,head . ,tail) tree)
         (conde
           [(fresh (a d head^ tail^)
              (== `(,a . ,d) head)
              (rember*o head head^)
              (rember*o tail tail^)
              (== `(,head^ . ,tail^) out))]
           [(== head 8) (rember*o tail out)]
           [(fresh (tail^)
              (rember*o tail tail^)
              (== `(,head . ,tail^) out))]))]))

  (list
   (list 'three-branches 10
         (fresh (q) (conde [(== q 'a)] [(== q 'b)] [(== q 'c)])))
   (list 'infinite-answers 10 (call/fresh fives))
   (list 'infinite-left-pauses 1
         (call/fresh (λ (q) (disj never (== q 'ok)))))
   (list 'infinite-right-pauses 1
         (call/fresh (λ (q) (disj (== q 'ok) never))))
   (list 'infinite-and-finite 10
         (call/fresh (λ (q) (disj (fives q) (== q 6)))))
   (list 'appendo-splits 10
         (fresh (q left right)
           (== `(,left ,right) q)
           (appendo left right '(1 2 3 4 5))))
   (list 'appendo-generate 10
         (fresh (q left right out)
           (== `(,left ,right ,out) q)
           (appendo left right out)))
   (list 'rember-generate 8
         (fresh (q) (rember*o q '(1 2 8 3 4 5))))))
