#lang racket/base

(require racket/stream racket/promise rackunit "stream-api.rkt")
(provide racket-stream-api suspend-stream stream-suspension?
         stream-force-one stream-head-forced? stream-rest/raw)

;; Experimental private dependency, isolated here. No private field is mutated.
(require/expose racket/private/stream-cons
  (eagerly-created-stream? eagerly-created-stream-first-forced?
   eagerly-created-stream-rest))

;; Remember a logical pause even after ordinary stream-force compresses its
;; native delay chain. Ephemeron keys let discarded streams be collected.
(define steps (make-ephemeron-hasheq))
(define (suspend-stream resume)
  (define step (delay (resume)))
  (define source (stream-lazy (force step)))
  (hash-set! steps source step)
  source)

(define (stream-suspension? source) (hash-has-key? steps source))
(define (stream-force-one source)
  (force (hash-ref steps source
                   (lambda () (error 'stream-force-one
                                     "not an experimental suspension: ~e" source)))))
(define (stream-head-forced? source)
  (and (eagerly-created-stream? source)
       (eagerly-created-stream-first-forced? source)))
(define (stream-rest/raw source)
  (eagerly-created-stream-rest source))

(define (emit value rest) (stream-cons #:eager value #:eager rest))
(define (inspect source)
  (cond
    [(stream-suspension? source) (paused (lambda () (stream-force-one source)))]
    [(null? source) (end)]
    [(stream-head-forced? source) (ready (stream-first source) (stream-rest/raw source))]
    [else (error 'inspect "expected a stream built by the experimental constructors: ~e"
                 source)]))
(define racket-stream-api
  (make-stream-api (representation '() emit suspend-stream inspect)))
