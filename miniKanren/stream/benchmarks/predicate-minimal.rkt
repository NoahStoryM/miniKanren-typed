#lang typed/racket/base

(define-type Tree (U Symbol Null (Pairof Tree Tree)))
(define-predicate generated? Tree)

(: handwritten? (-> Any Boolean : Tree))
(define (handwritten? x)
  (or (symbol? x)
      (null? x)
      (and (pair? x)
           (handwritten? (car x))
           (handwritten? (cdr x)))))

(define input (read (open-input-string "(a b c d e f g h i j k l)")))

(: bench (-> (-> Any Boolean) Void))
(define (bench p)
  (for ([i (in-range 10000)]) (unless (p input) (error "wrong result")))
  (collect-garbage)
  (collect-garbage)
  (time
   (for ([i (in-range 1000000)])
     (unless (p input) (error "wrong result")))))

(for ([i (in-range 3)])
  (displayln 'generated)
  (bench generated?)
  (displayln 'handwritten)
  (bench handwritten?))
