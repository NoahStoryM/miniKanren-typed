#lang racket/base

;; Preserve stock consumers and derived operators. Replace the constructors
;; and full force together, and add the non-forcing queries and single-step operation.
(require (except-in racket/stream stream-cons stream-lazy stream-force)
         "private/stream-cons.rkt")
(provide (all-from-out racket/stream)
         (all-from-out "private/stream-cons.rkt"))
