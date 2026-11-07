#lang racket/base

;; Experimental, untyped microKanren using Racket's native streams.
(require experiments/search-stream/private/stream-api experiments/search-stream/private/representation
         "private/core.rkt" "private/terms.rkt")
(provide (all-defined-out) (all-from-out "private/terms.rkt"))

(define engine (make-microkanren racket-stream-api))
(define == (kanren-== engine))
(define call/fresh (kanren-call/fresh engine))
(define disj (kanren-disj engine))
(define conj (kanren-conj engine))
(define delay-goal (kanren-delay-goal engine))
(define take (stream-api-take (kanren-streams engine)))
(define take-all (stream-api-take-all (kanren-streams engine)))

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
    [(_ (first rest ...) goal ...)
     (call/fresh (λ (first) (fresh (rest ...) goal ...)))]))
(define-syntax-rule (conde [first rest ...] ...)
  (disj+ (conj+ first rest ...) ...))
(define (call/goal goal) (goal empty-state))
(define-syntax-rule (run n (x ...) first rest ...)
  (map reify-1st (take n (call/goal (fresh (x ...) first rest ...)))))
(define-syntax-rule (run* (x ...) first rest ...)
  (map reify-1st (take-all (call/goal (fresh (x ...) first rest ...)))))
