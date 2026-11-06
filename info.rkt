#lang info

(define license 'MIT)
(define collection "typed")
(define version "0.0")

(define pkg-desc "Typed miniKanren")

(define deps '("base" "typed-racket-lib"))
(define build-deps
  '("at-exp-lib"
    "rackunit-typed"
    "scribble-lib"
    "racket-doc"
    "typed-racket-doc"))

(define scribblings '(("scribblings/miniKanren.scrbl" (multi-page))))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))

