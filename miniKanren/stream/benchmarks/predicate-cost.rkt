#lang typed/racket/base

;; Compile before running. Both predicates and the timers are in the same
;; checked module, with no typed/untyped boundary inside the measured loop.
(define-type Literal (U Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Data (U Literal (Pair Data Data)))
(define-predicate generated-data? Data)

(: manual-data? (-> Any Boolean : Data))
(define (manual-data? v)
  (or (boolean? v) (number? v) (char? v) (bytes? v) (string? v)
      (keyword? v) (null? v) (symbol? v)
      (and (pair? v) (manual-data? (car v)) (manual-data? (cdr v)))))

(define specimen : Data '(a b c d e f g h i j k l))
(define count : Natural 100000)
(: exercise (-> (-> Any Boolean) Integer))
(define (exercise predicate)
  (for/fold ([hits : Integer 0]) ([i (in-range count)])
    (if (predicate specimen) (add1 hits) hits)))

(: measure (-> (-> Any Boolean) (Listof Integer)))
(define (measure predicate)
  (void (exercise predicate))
  (for/list : (Listof Integer) ([i (in-range 7)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (define start (current-process-milliseconds))
    (define hits (exercise predicate))
    (define elapsed (- (current-process-milliseconds) start))
    (unless (= hits count) (error 'measure "predicate result differs"))
    elapsed))

(displayln (list 'generated (measure generated-data?)))
(displayln (list 'manual (measure manual-data?)))
