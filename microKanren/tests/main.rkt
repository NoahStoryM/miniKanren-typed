#lang typed/racket/base

(require racket/include
         typed/rackunit
         "../main.rkt")

;; Helpers for the tests that look into streams directly.  The original
;; tests apply car, cdr, and a suspension to a stream whose shape they
;; know; here the shape is checked first.

(: $car (∀ (a) (→ (Streamof a) a)))
(define ($car $)
  (cond
    [(stream-empty? $) (error '$car "empty stream")]
    [else
     (define a (stream-first $))
     (if (pause? a) (error '$car "not a mature stream: ~e" $) a)]))

(: $cdr (∀ (a) (→ (Streamof a) (Streamof a))))
(define ($cdr $)
  (cond
    [(stream-empty? $) (error '$cdr "empty stream")]
    [(pause? (stream-first $)) (error '$cdr "not a mature stream: ~e" $)]
    [else (stream-rest $)]))

(: $force (∀ (a) (→ (Streamof a) (Streamof a))))
(define ($force $)
  (cond
    [(stream-empty? $) (error '$force "empty stream")]
    [(pause? (stream-first $)) (stream-rest $)]
    [else (error '$force "not an immature stream: ~e" $)]))

(include "programs.rktl")
(include "core.rktl")
(include "wrappers.rktl")
