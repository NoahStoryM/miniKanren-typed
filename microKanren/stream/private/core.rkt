#lang racket/base

(require experiments/search-stream/private/stream-api "terms.rkt")
(provide (struct-out kanren) make-microkanren)

(struct kanren (streams == call/fresh disj conj delay-goal) #:transparent)

;; Goal = State -> S State. This layer never inspects a stream, invokes
;; a stream thunk, calls force, or tests its underlying representation.
(define (make-microkanren streams)
  (define empty (stream-api-empty streams))
  (define unit (stream-api-unit streams))
  (define suspend (stream-api-suspend streams))
  (define merge (stream-api-merge streams))
  (define bind (stream-api-bind streams))

  (define ((== left right) state)
    (define substitution (unify left right (car state)))
    (if substitution (unit (cons substitution (cdr state))) empty))

  (define ((call/fresh make-goal) state)
    (define counter (cdr state))
    ((make-goal (var counter)) (cons (car state) (add1 counter))))

  (define ((disj left right) state)
    (merge (left state) (right state)))

  (define ((conj left right) state)
    (bind (left state) right))

  ;; Delay constructing AND invoking a recursive goal, not only its result.
  (define ((delay-goal make-goal) state)
    (suspend (λ () ((make-goal) state))))

  (kanren streams == call/fresh disj conj delay-goal))
