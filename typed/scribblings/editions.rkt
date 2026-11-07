#lang racket/base

;; Typesetting helpers for the comparison section, whose code comes from
;; both editions.  The two modules export many of the same names, so a
;; single for-label require cannot link them all correctly.  Instead,
;; each submodule below has the for-label bindings of one edition, and
;; its macros give the code they typeset the lexical context of that
;; submodule: identifiers in (trs1 ...) link to typed/miniKanren/trs1,
;; and those in (trs2 ...) link to typed/miniKanren/trs2.

(module recontext racket/base
  (provide recontext)
  ;; Rebuild stx with the lexical context of ctx, keeping the source
  ;; locations (which determine the layout) and the properties.
  (define (recontext ctx stx)
    (let loop ([s stx])
      (define e (syntax-e s))
      (datum->syntax
       ctx
       (cond
         [(pair? e)
          (let loop* ([p e])
            (cond
              [(pair? p) (cons (loop (car p)) (loop* (cdr p)))]
              [(syntax? p) (loop p)]
              [else p]))]
         [(vector? e) (list->vector (map loop (vector->list e)))]
         [(box? e) (box (loop (unbox e)))]
         [else e])
       s
       s))))

(define-syntax-rule (define-edition name examples-name codeblock-name mod)
  (module name racket/base
    (require scribble/manual
             scribble/example
             (for-syntax racket/base (submod ".." recontext))
             (for-label typed/racket/base mod))
    (provide name examples-name codeblock-name)
    (define the-eval
      (make-base-eval #:lang 'typed/racket/base '(require mod)))
    (define-syntax (examples-name stx)
      (syntax-case stx ()
        [(_ e (... ...))
         (with-syntax ([(e* (... ...))
                        (for/list ([e (in-list (syntax->list #'(e (... ...))))])
                          (recontext #'here e))])
           #'(examples #:eval the-eval e* (... ...)))]))
    (define-syntax (codeblock-name stx)
      (syntax-case stx ()
        [(_ e (... ...))
         (with-syntax ([(e* (... ...))
                        (for/list ([e (in-list (syntax->list #'(e (... ...))))])
                          (recontext #'here e))])
           #'(racketblock e* (... ...)))]))
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ e) (with-syntax ([e* (recontext #'here #'e)])
                 #'(racket e*))]))))

(define-edition trs1 trs1-examples trs1-block typed/miniKanren/trs1)
(define-edition trs2 trs2-examples trs2-block typed/miniKanren/trs2)

(require 'trs1 'trs2)
(provide (all-from-out 'trs1 'trs2))
