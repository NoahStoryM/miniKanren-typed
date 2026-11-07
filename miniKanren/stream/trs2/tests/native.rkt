#lang racket/base

(require rackunit
         (prefix-in mk: miniKanren/stream/trs2)
         (prefix-in native: private/racket/stream))

;; Relation suspension is the new native delay, not the historical side table.
(mk:defrel (oliveo q) (mk:== q 'olive))
(define q (mk:var 'q))
(define source ((oliveo q) mk:empty-s))
(check-true (native:stream? source))
(check-false (native:stream-empty/known? source))
(check-false (native:stream-first-forced? source))
(define resumed (native:stream-force-once source))
(check-true (native:stream-first-forced? resumed))
(check-equal? (map (mk:reify q) (mk:take-inf #f source)) '(olive))
(check-true (native:stream-first-forced? source))
(check-eq? (mk:fail mk:empty-s) native:empty-stream)
(check-true (native:stream-empty/known? (mk:fail mk:empty-s)))

;; Consecutive pauses must remain single steps for ifte and once.
(define ((twice goal) s)
  (native:stream-lazy (native:stream-lazy (goal s))))
(check-equal?
 (mk:run* q
   (mk:ifte (twice mk:fail) mk:succeed (mk:== q 'fallback)))
 '(fallback))
(check-equal?
 (mk:run* q
   (mk:ifte (twice (mk:disj2 (mk:== q 'olive) (mk:== q 'oil)))
            mk:succeed (mk:== q 'fallback)))
 '(olive oil))
(check-equal?
 (mk:run* q
   (mk:once (twice (mk:disj2 (mk:== q 'olive) (mk:== q 'oil)))))
 '(olive))

(define tail-calls 0)
(define (delayed-tail s)
  (native:stream-lazy
   (begin (set! tail-calls (add1 tail-calls)) (mk:succeed s))))
(check-equal?
 (mk:run* q (mk:once (mk:disj2 (mk:== q 'first) delayed-tail)))
 '(first))
(check-equal? tail-calls 0)

(mk:defrel (nevero) (nevero))
(check-equal? (mk:run 1 q (mk:disj2 (nevero) (mk:== q 'ok))) '(ok))
