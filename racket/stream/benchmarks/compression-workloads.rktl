;; Included by both primitive versions; measures new API operations only.
(provide exercise)
(define (chain n)
  (if (zero? n) (stream-cons #:eager 1 #:eager empty-stream)
      (stream-lazy (chain (sub1 n)))))
(define (exercise mode n)
  (define root (chain n))
  (define cursor
    (for/fold ([cursor root]) ([i (in-range n)])
      (stream-force-one (if (eq? mode 'root) root cursor))))
  (unless (eq? (stream-inspect cursor) 'ready) (error 'exercise "not ready"))
  (unless (= (stream-first (stream-force root)) 1) (error 'exercise "bad root"))
  n)
