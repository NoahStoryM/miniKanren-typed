#lang racket/base

;; Suspensions as explicit stream elements (RESEARCH, ambKanren issue #14).
;;
;; The *-pause Kanren ports use only racket/stream.  Every search stream
;; has an evaluated head; it is one of
;;
;;   empty-stream
;;   (stream-cons #:eager answer #:eager rest)   ; a ready answer
;;   (stream-cons #:eager PAUSE  rest)           ; a suspension (lazy rest)
;;
;; so the original three-way case analysis (empty / ready / immature)
;; is `stream-empty?` plus a look at `stream-first`, and never forces a
;; suspension.  Taking the rest of a PAUSE node is what calling a
;; suspension thunk did in the original code.  The suspended expression
;; runs when that rest is first inspected (`stream-empty?`,
;; `stream-first`), which can be later than the original call; the
;; sequence of answers and pauses is the same.
;;
;; Unlike a thunk, the rest is memoized.  A PAUSE node stays a PAUSE
;; node after its rest is forced, so a reused stream presents the same
;; answers and pauses every time it is traversed.

(require racket/stream)
(provide PAUSE suspend paused?)

;; The marker.  It is uninterned, so no answer can be mistaken for it.
(define PAUSE (string->uninterned-symbol "PAUSE"))

;; A suspension of `e`, which must produce a search stream.
(define-syntax-rule (suspend e) (stream-cons #:eager PAUSE e))

;; Is the non-empty search stream `s` a suspension?
(define (paused? s) (eq? (stream-first s) PAUSE))
