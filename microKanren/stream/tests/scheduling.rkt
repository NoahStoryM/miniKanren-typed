#lang racket/base

(module reference typed/racket/base
  (require typed/racket/unsafe "../../main.rkt")
  ;; Use original functions without higher-order boundary contracts.
  (unsafe-provide unit mzero mplus bind == call/fresh disj conj
                  empty-state take var? var-c))

(require rackunit racket/match racket/promise racket/stream
         "../private/stream-api.rkt" "../private/representation.rkt"
         "../private/core.rkt" "workloads.rkt"
         (prefix-in native: "../private/representation.rkt")
         (prefix-in term: "../private/terms.rkt")
         (prefix-in syntax: "../main.rkt")
         (prefix-in ref: (submod "." reference)))

(define (reference-trace n source [project values])
  (cond
    [(zero? n) '()]
    [(null? source) '(end)]
    [(procedure? source)
     (cons 'pause
           (if (= n 1) '() (reference-trace (sub1 n) (source) project)))]
    [else
     (cons (list 'answer (project (car source)))
           (if (= n 1) '()
               (reference-trace (sub1 n) (cdr source) project)))]))

;; An independent interpreter uses the original merge/bind, not the API.
(define (oracle expression input)
  (match expression
    ['empty ref:mzero]
    ['unit (ref:unit input)]
    [`(add ,n) (ref:unit (+ input n))]
    [`(pause ,body) (λ () (oracle body input))]
    [`(merge ,left ,right)
     (ref:mplus (oracle left input) (oracle right input))]
    [`(bind ,source ,next)
     (ref:bind (oracle source input) (λ (x) (oracle next x)))]))

(define (interpret expression input api)
  (match expression
    ['empty (stream-api-empty api)]
    ['unit ((stream-api-unit api) input)]
    [`(add ,n) ((stream-api-unit api) (+ input n))]
    [`(pause ,body) (suspend/with api (interpret body input api))]
    [`(merge ,left ,right)
     ((stream-api-merge api) (interpret left input api) (interpret right input api))]
    [`(bind ,source ,next)
     ((stream-api-bind api) (interpret source input api)
                            (λ (x) (interpret next x api)))]))

(define (generate depth)
  (if (zero? depth)
      (list-ref '(empty unit (add 1) (add -1)) (random 4))
      (case (random 4)
        [(0) (generate 0)]
        [(1) `(pause ,(generate (sub1 depth)))]
        [(2) `(merge ,(generate (sub1 depth)) ,(generate (sub1 depth)))]
        [(3) `(bind ,(generate (sub1 depth)) ,(generate (sub1 depth)))])))

(parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
  (random-seed 20261107)
  (for ([i (in-range 500)])
    (define expression (generate 5))
    (define expected-trace (reference-trace 300 (oracle expression 0)))
    (define expected-answers (ref:take 40 (oracle expression 0)))
    (for ([api (in-list (list racket-stream-api))])
      (define source (interpret expression 0 api))
      (check-equal? ((stream-api-trace api) 300 source)
                    expected-trace (format "trace: ~s" expression))
      (check-equal? ((stream-api-take api) 40 source)
                    expected-answers (format "answers: ~s" expression))
      ;; Depth five has at most 65536 answers. Read all of them through
      ;; stock Racket stream operations, then re-observe cached pauses.
      (check-equal? (stream->list source)
                    (ref:take 65537 (oracle expression 0))
                    (format "ordinary stream consumption: ~s" expression))
      (check-equal? ((stream-api-trace api) 300 source)
                    expected-trace (format "cached trace: ~s" expression)))))

;; Make variable representations comparable without sharing term code.
(define (normalize value)
  (cond
    [(term:var? value) (list 'var (term:var-c value))]
    [(ref:var? value) (list 'var (ref:var-c value))]
    [(pair? value) (cons (normalize (car value)) (normalize (cdr value)))]
    [else value]))

(define reference-programs
  (workloads ref:== ref:call/fresh ref:disj ref:conj
             (λ (make-goal) (λ (state) (λ () ((make-goal) state))))))

(for ([api (in-list (list racket-stream-api))]
      [name '(native)])
  (define engine (make-microkanren api))
  (define programs
    (workloads (kanren-== engine) (kanren-call/fresh engine)
               (kanren-disj engine) (kanren-conj engine)
               (kanren-delay-goal engine)))
  (for ([reference (in-list reference-programs)]
        [program (in-list programs)])
    (define goal (caddr program))
    (define reference-goal (caddr reference))
    (define label (format "~a: ~a" name (car program)))
    (check-equal?
     ((stream-api-trace api) 250 (goal term:empty-state) normalize)
     (reference-trace 250 (reference-goal ref:empty-state) normalize) label)
    (check-equal?
     (normalize ((stream-api-take api) (cadr program) (goal term:empty-state)))
     (normalize (ref:take (cadr reference) (reference-goal ref:empty-state))) label))
  (printf "~a: eight recursive programs match original states and pause traces\n" name)

  ;; Constructor and observer contracts: observation is not execution.
  (define calls 0)
  (define source (suspend/with api (set! calls (add1 calls))
                                    ((stream-api-unit api) 'a)))
  (check-equal? calls 0)
  (check-true (paused? ((stream-api-inspect api) source)))
  (check-equal? calls 0)
  (check-equal? ((stream-api-take api) 0 source) '())
  (check-equal? calls 0)
  (define continued ((paused-resume ((stream-api-inspect api) source))))
  (check-equal? calls 1)
  (check-equal? ((stream-api-take-all api) continued) '(a))
  ;; Re-observation preserves the pause node but reuses its cached result.
  ((paused-resume ((stream-api-inspect api) source)))
  (check-equal? calls 1)

  ;; Preserve eager construction of ready prefixes.
  (define order '())
  (define merged
    ((stream-api-merge api) ((stream-api-unit api) 1)
                            ((stream-api-unit api) 2)))
  ((stream-api-bind api) merged
   (λ (x) (set! order (cons x order)) ((stream-api-unit api) x)))
  (check-equal? (reverse order) '(1 2))
  (check-equal? ((stream-api-trace api) 10 merged)
                '((answer 1) (answer 2) end))

  ;; A cooperatively infinite branch must not starve a sibling.
  (define (never) (suspend/with api (never)))
  (check-equal?
   ((stream-api-take api) 1
    ((stream-api-merge api) (never) ((stream-api-unit api) 42))) '(42)))

(check-equal? (syntax:run* (q) (syntax:conde [(syntax:== q 'a)]
                                          [(syntax:== q 'b)])) '(a b))
(check-equal?
 (let ()
   (define (appendo left right out)
     (syntax:conde
       [(syntax:== left '()) (syntax:== right out)]
       [(syntax:fresh (head tail result)
          (syntax:== left `(,head . ,tail))
          (syntax:== out `(,head . ,result))
          (appendo tail right result))]))
   (syntax:run* (q)
     (syntax:fresh (left right)
       (syntax:== q `(,left ,right))
       (appendo left right '(1 2)))))
 '((() (1 2)) ((1) (2)) ((1 2) ())))

;; The native backend is a standard stream and stores states directly.
(define state (cons '() 0))
(define native-ready ((stream-api-unit racket-stream-api) state))
(check-true (stream? native-ready))
(check-eq? (stream-first native-ready) state)
(check-true (stream-empty? (stream-rest native-ready)))
(define suspension (suspend/with racket-stream-api native-ready))
(check-true (native:stream-suspension? suspension))
(define promised-value (delay (error 'answer "must not be forced")))
(check-eq? (car ((stream-api-take racket-stream-api) 1
                 ((stream-api-unit racket-stream-api) promised-value))) promised-value)
(check-false (promise-forced? promised-value))
