#lang typed/racket/base

(require racket/include
         typed/rackunit
         "../main.rkt")

;; Helpers for the tests that look into streams directly.  The original
;; tests apply car, cdr, and a suspension to a stream whose shape they
;; know; here the shape is checked first.

(: $car (∀ (a) (→ (Streamof a) a)))
(define ($car $)
  (if (pair? $) (car $) (error '$car "not a mature stream: ~e" $)))

(: $cdr (∀ (a) (→ (Streamof a) (Streamof a))))
(define ($cdr $)
  (if (pair? $) (cdr $) (error '$cdr "not a mature stream: ~e" $)))

(: $force (∀ (a) (→ (Streamof a) (Streamof a))))
(define ($force $)
  (if (procedure? $) ($) (error '$force "not an immature stream: ~e" $)))

(include "programs.rktl")
(include "core.rktl")
(include "wrappers.rktl")
