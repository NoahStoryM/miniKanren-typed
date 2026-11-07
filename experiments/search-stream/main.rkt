#lang racket/base

;; Experimental search streams. Values are ordinary racket/stream streams;
;; construction must go through this API to preserve scheduling boundaries.
(require experiments/search-stream/private/stream-api experiments/search-stream/private/representation)
(provide stream-empty stream-unit stream-suspend stream-inspect
         stream-merge stream-bind stream-pull stream-take stream-take-all
         stream-trace stream-suspension? stream-force-one
         stream-head-forced? stream-rest/raw
         (struct-out end) (struct-out ready) (struct-out paused))

(define stream-empty (stream-api-empty racket-stream-api))
(define stream-unit (stream-api-unit racket-stream-api))
(define-syntax-rule (stream-suspend body ...)
  (suspend-stream (lambda () body ...)))
(define stream-inspect (stream-api-inspect racket-stream-api))
(define stream-merge (stream-api-merge racket-stream-api))
(define stream-bind (stream-api-bind racket-stream-api))
(define stream-pull (stream-api-pull racket-stream-api))
(define stream-take (stream-api-take racket-stream-api))
(define stream-take-all (stream-api-take-all racket-stream-api))
(define stream-trace (stream-api-trace racket-stream-api))
