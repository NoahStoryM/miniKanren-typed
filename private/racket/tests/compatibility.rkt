#lang racket/base
(module stock racket/base
  (require racket/include racket/stream racket/contract racket/match rackunit)
  (include "compatibility.rktl"))
(module copied racket/base
  (require racket/include private/racket/stream racket/contract racket/match rackunit)
  (include "compatibility.rktl"))
(require (submod "." stock) (submod "." copied)
         rackunit (prefix-in stock: racket/stream)
         (prefix-in copied: private/racket/stream))
;; Stock generic consumers accept copied native nodes and conversely.
(check-equal? (stock:stream->list (copied:stream 1 2 3)) '(1 2 3))
(check-equal? (copied:stream->list (stock:stream 1 2 3)) '(1 2 3))
;; Every stock export must remain present (the extension may add exports).
(define (exports path)
  (define-values (values syntax) (module->exports path))
  (apply append
         (for/list ([phases (in-list (list values syntax))])
           (define entry (assv 0 phases))
           (if entry (map car (cdr entry)) '()))))
(define copied-names (exports 'private/racket/stream))
(for ([name (in-list (exports 'racket/stream))])
  (check-not-false (memq name copied-names)))
