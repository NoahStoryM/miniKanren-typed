;; Included unchanged by both the stock and fork submodules.
(provide exercise retained-memory)

(define kept #f)
(define (gc!) (collect-garbage) (collect-garbage) (collect-garbage))

(define (eager-sequence n)
  (let loop ([i n] [rest empty-stream])
    (if (zero? i) rest
        (loop (sub1 i) (stream-cons #:eager i #:eager rest)))))

(define (lazy-tail-sequence n)
  (if (zero? n) empty-stream
      (stream-cons #:eager n (lazy-tail-sequence (sub1 n)))))

(define (lazy-both-sequence n)
  (if (zero? n) empty-stream
      (stream-cons (+ n 1) (lazy-both-sequence (sub1 n)))))

(define (delay-chain n)
  (if (zero? n)
      (stream-cons #:eager 1 #:eager empty-stream)
      (stream-lazy (delay-chain (sub1 n)))))

(define (consume source)
  (let loop ([source source] [sum 0])
    (if (stream-empty? source) sum
        (loop (stream-rest source) (+ sum (stream-first source))))))

(define (exercise kind n)
  (case kind
    [(eager) (consume (eager-sequence n))]
    [(lazy-tail) (consume (lazy-tail-sequence n))]
    [(lazy-both) (consume (lazy-both-sequence n))]
    [(delay-chain) (stream-first (stream-force (delay-chain n)))]
    [(cached-head)
     (define source (stream-lazy (stream-cons #:eager 1 #:eager empty-stream)))
     (void (stream-first source))
     (for/fold ([sum 0]) ([i (in-range n)]) (+ sum (stream-first source)))]
    [else (error 'exercise "unknown workload: ~a" kind)]))

;; Retained managed-heap delta after full collections, not peak RSS.
;; kept remains a module root across the measurement in both variants.
(define (retained-memory kind n)
  (set! kept #f)
  (gc!)
  (define before (current-memory-use))
  (set! kept (case kind
               [(delay-chain) (delay-chain n)]
               [(lazy-tail) (lazy-tail-sequence n)]
               [else (error 'retained-memory "unknown workload")]))
  (case kind
    [(delay-chain) (void (stream-force kept))]
    [(lazy-tail) (void (consume kept))])
  (gc!)
  (define after (current-memory-use))
  ;; Keep the source live past the memory observation.
  (unless (stream? kept) (error 'retained-memory "lost source"))
  (set! kept #f)
  (gc!)
  (- after before))
