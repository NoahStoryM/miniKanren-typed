#lang typed/racket/no-check

;; Answer/pause traces of the *-pause ports against the original
;; pair/thunk implementations, on random stream expressions.
(require racket/match
         rackunit
         racket/stream
         private/stream-pause
         (prefix-in micro: microKanren/stream-pause)
         (prefix-in trs1: miniKanren/stream-pause/trs1)
         (prefix-in trs2: miniKanren/stream-pause/trs2)
         (prefix-in old-micro: typed/microKanren)
         (prefix-in old-trs1: typed/miniKanren/trs1)
         (prefix-in old-trs2: typed/miniKanren/trs2))

;; These interpreters are test oracles only.
(define (reference expression merge bind)
  (match expression
    ['empty '()]
    [`(emit ,v ,rest) (cons v (reference rest merge bind))]
    [`(pause ,rest) (lambda () (reference rest merge bind))]
    [`(merge ,left ,right)
     (merge (reference left merge bind) (reference right merge bind))]
    [`(bind ,rest)
     (bind (reference rest merge bind)
           (lambda (v) (cons v (lambda () (list (+ v 10))))))]))

(define (native expression merge bind)
  (match expression
    ['empty empty-stream]
    [`(emit ,v ,rest)
     (stream-cons #:eager v #:eager (native rest merge bind))]
    [`(pause ,rest) (suspend (native rest merge bind))]
    [`(merge ,left ,right)
     (merge (native left merge bind) (native right merge bind))]
    [`(bind ,rest)
     (bind (native rest merge bind)
           (lambda (v)
             (stream-cons #:eager v
                          #:eager (suspend
                                   (stream-cons #:eager (+ v 10)
                                                #:eager empty-stream)))))]))

(define (reference-trace s)
  (cond
    [(null? s) '(end)]
    [(pair? s) (cons `(answer ,(car s)) (reference-trace (cdr s)))]
    [else (cons 'pause (reference-trace (s)))]))

(define (native-trace s)
  (cond
    [(stream-empty? s) '(end)]
    [(paused? s) (cons 'pause (native-trace (stream-rest s)))]
    [else (cons `(answer ,(stream-first s)) (native-trace (stream-rest s)))]))

(define (expression depth)
  (if (zero? depth)
      (if (zero? (random 2)) 'empty `(emit ,(random 5) empty))
      (case (random 5)
        [(0) 'empty]
        [(1) `(emit ,(random 5) ,(expression (sub1 depth)))]
        [(2) `(pause ,(expression (sub1 depth)))]
        [(3) `(merge ,(expression (sub1 depth)) ,(expression (sub1 depth)))]
        [else `(bind ,(expression (sub1 depth)))])))

(parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
  (random-seed 20261107)
  (for ([i (in-range 250)])
    (define e (expression 5))
    (check-equal?
     (native-trace (native e micro:mplus micro:bind))
     (reference-trace (reference e old-micro:mplus old-micro:bind)))
    (check-equal?
     (native-trace
      (native e trs2:append-inf (lambda (s g) (trs2:append-map-inf g s))))
     (reference-trace
      (reference e old-trs2:append-inf (lambda (s g) (old-trs2:append-map-inf g s)))))))

;; TRS1 suspends only choice tails. Both depth-first and interleaved
;; operators retain their original answer order.
(for ([n (in-range 30)])
  (define (old-row v)
    (cons v (lambda () (box-immutable (+ v 10)))))
  (define (row v)
    (trs1:choice v (trs1:λF : Integer (trs1:unit (+ v 10)))))
  (check-equal?
   (trs1:map∞ #f values (trs1:mplus (row n) (trs1:λF : Integer (row (+ n 1)))))
   (old-trs1:map∞ #f values
                  (old-trs1:mplus (old-row n) (lambda () (old-row (+ n 1))))))
  (check-equal?
   (trs1:map∞ #f values (trs1:mplusi (row n) (trs1:λF : Integer (row (+ n 1)))))
   (old-trs1:map∞ #f values
                  (old-trs1:mplusi (old-row n) (lambda () (old-row (+ n 1))))))
  (check-equal? (trs1:map∞ #f values (trs1:bind (row n) row))
                (old-trs1:map∞ #f values (old-trs1:bind (old-row n) old-row)))
  (check-equal? (trs1:map∞ #f values (trs1:bindi (row n) row))
                (old-trs1:map∞ #f values (old-trs1:bindi (old-row n) old-row))))

;; A ready answer does not run its suspended tail.
(define tail-calls 0)
(define tail (suspend (begin (set! tail-calls (add1 tail-calls)) empty-stream)))
(define ready (stream-cons #:eager #f #:eager tail))
(check-equal? (micro:take 1 ready) '(#f))
(check-equal? (trs2:take-inf 1 ready) '(#f))
(check-equal? tail-calls 0)
(check-equal? (trs1:map∞ 1 values (trs1:choice #f tail)) '(#f))
(check-equal? tail-calls 0)

;; Unit's tail is empty; a choice's tail is a PAUSE node, and stays one
;; even after it has been forced to empty.
(check-equal? (trs1:map∞ #f values (trs1:unit '(a b))) '((a b)))
;; `stream-rest` alone does not run the suspension; inspecting it does.
(void (stream-rest tail))
(check-equal? tail-calls 0)
(void (stream-empty? (stream-rest tail)))
(check-equal? tail-calls 1)
(void (stream-empty? (stream-rest tail)))
(check-equal? tail-calls 1)
(check-equal? (trs1:case∞ (trs1:choice 'a tail)
                [() 'empty] [(a) 'unit] [(a f) 'choice])
              'choice)

;; Consecutive suspensions remain separate scheduling events.
(check-equal? (native-trace (suspend (suspend empty-stream))) '(pause pause end))
(define (never) (suspend (never)))
(check-equal? (micro:take 1 (micro:mplus (never) (micro:unit 'right))) '(right))
(check-equal? (trs2:take-inf 1
                             (trs2:append-inf (never)
                                              (stream-cons #:eager 'right #:eager empty-stream)))
              '(right))

;; Memoization does not change scheduling: a forced PAUSE node is still a
;; PAUSE node, so a reused stream merges the same way every time.
(define warmed (suspend (micro:unit 'left)))
(check-equal? (micro:take 2 (micro:mplus warmed (micro:unit 'right))) '(right left))
(check-equal? (micro:take 2 (micro:mplus warmed (micro:unit 'right))) '(right left))
(define cached-empty (suspend (suspend empty-stream)))
(void (micro:take-all cached-empty))
(check-equal? (native-trace (micro:mplus cached-empty (micro:unit 'right)))
              '(pause (answer right) pause end))

;; ifte and once preserve each leading pause, and once leaves the unused
;; tail untouched.
(define (test-goal s)
  (suspend (suspend (stream-cons #:eager 1 #:eager empty-stream))))
(define (then-goal v)
  (stream-cons #:eager (+ v 10) #:eager empty-stream))
(define else-calls 0)
(define (else-goal s)
  (set! else-calls (add1 else-calls))
  (stream-cons #:eager 'else #:eager empty-stream))
(check-equal? (native-trace ((trs2:ifte test-goal then-goal else-goal) '()))
              '(pause pause (answer 11) end))
(check-equal? else-calls 0)
(check-equal? (native-trace ((trs2:once test-goal) '()))
              '(pause pause (answer 1) end))
(check-equal? (native-trace ((trs2:ifte trs2:fail then-goal else-goal) '()))
              '((answer else) end))
(check-equal? else-calls 1)
(check-true (stream-empty? ((trs2:once trs2:fail) '())))
(define unused-calls 0)
(define unused-tail (suspend (begin (set! unused-calls (add1 unused-calls)) empty-stream)))
(check-equal?
 (trs2:take-inf #f
                ((trs2:once (lambda (s)
                              (stream-cons #:eager #f #:eager unused-tail)))
                 '()))
 '(#f))
(check-equal? unused-calls 0)

;; An answer equal to the marker's name is still an answer.
(check-equal? (native-trace (stream-cons #:eager 'PAUSE #:eager empty-stream))
              '((answer PAUSE) end))
