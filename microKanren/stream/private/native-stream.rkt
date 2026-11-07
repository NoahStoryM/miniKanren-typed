#lang racket/base

(require racket/stream/main "stream-api.rkt")
(provide racket-stream-api suspend-stream)

;; The four search-stream primitives. Answer heads and tail objects are eager;
;; every pause uses the extended stream-lazy. No private fields or side tables.
(define (emit answer rest)
  (stream-cons #:eager answer #:eager rest))
(define (suspend-stream resume)
  (stream-lazy (resume)))
(define (inspect source)
  (cond
    [(stream-empty/known? source) (end)]
    [(stream-first-forced? source)
     (ready (stream-first source) (stream-rest source))]
    [else
     (paused
      (lambda ()
        (define next (stream-force-once source))
        (when (eq? next source)
          (error 'search-stream "expected an eager answer or a local delayed stream"))
        next))]))

(define racket-stream-api
  (make-stream-api (representation empty-stream emit suspend-stream inspect)))
