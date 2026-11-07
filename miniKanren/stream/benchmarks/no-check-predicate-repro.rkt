#lang racket/base

;; Racket 9.3 CS: changing only the language changes this predicate's result.
(module checked typed/racket/base
  (provide probe)
  (define-predicate integer-value? Integer)
  (: probe (→ (Listof Boolean)))
  (define (probe) (map integer-value? (list #f 1))))

(module unchecked typed/racket/no-check
  (provide probe)
  (define-predicate integer-value? Integer)
  (: probe (→ (Listof Boolean)))
  (define (probe) (map integer-value? (list #f 1))))

(require (prefix-in checked: (submod "." checked))
         (prefix-in unchecked: (submod "." unchecked)))
(displayln (list 'checked (checked:probe)))
(displayln (list 'no-check (unchecked:probe)))
