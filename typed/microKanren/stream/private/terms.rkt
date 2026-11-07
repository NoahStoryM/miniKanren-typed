#lang racket/base

;; Untyped term operations adapted from ../../main.rkt.
;; Original microKanren: Hemann and Friedman, 2013, MIT. See NOTICE.
(require racket/list)
(provide (all-defined-out))

(struct var (c) #:transparent)
(define (var=? left right) (= (var-c left) (var-c right)))
(define empty-state (cons '() 0))

(define (walk value substitution)
  (define entry
    (and (var? value)
         (assf (λ (key) (var=? value key)) substitution)))
  (if entry (walk (cdr entry) substitution) value))

(define (ext-s variable value substitution)
  (cons (cons variable value) substitution))

;; Like microKanren, this unifier has no occurs check.
(define (unify left right substitution)
  (define u (walk left substitution))
  (define v (walk right substitution))
  (cond
    [(and (var? u) (var? v) (var=? u v)) substitution]
    [(var? u) (ext-s u v substitution)]
    [(var? v) (ext-s v u substitution)]
    [(and (pair? u) (pair? v))
     (define next (unify (car u) (car v) substitution))
     (and next (unify (cdr u) (cdr v) next))]
    [else (and (equal? u v) substitution)]))

(define (walk* value substitution)
  (define result (walk value substitution))
  (if (pair? result)
      (cons (walk* (car result) substitution)
            (walk* (cdr result) substitution))
      result))

(define (reify-s value substitution)
  (define result (walk value substitution))
  (cond
    [(var? result)
     (ext-s result
            (string->symbol (format "_.~a" (length substitution)))
            substitution)]
    [(pair? result)
     (reify-s (cdr result) (reify-s (car result) substitution))]
    [else substitution]))

(define (reify-1st state)
  (define value (walk* (var 0) (car state)))
  (walk* value (reify-s value '())))
