#lang racket/base

;; Isolated Racket 9.3 primitive fork, for comparison with stock streams.
;; Generic consumers are unchanged; only these primitives are substituted.
(require (except-in racket/stream stream-cons stream-lazy stream-force)
         (only-in "private/two-state-cons.rkt"
                  stream-cons stream-lazy stream-force
                  stream-inspect stream-force-one))
(provide (all-from-out racket/stream)
         stream-cons stream-lazy stream-force stream-inspect stream-force-one)
