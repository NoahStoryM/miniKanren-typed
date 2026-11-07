#lang info

(define license 'MIT)
(define collection 'multi)
(define version "1.1")

(define pkg-desc "Typed miniKanren")

(define deps '("base" "typed-racket-lib" "rackunit-lib"))
(define build-deps
  '("at-exp-lib"
    "rackunit-typed"
    "scribble-lib"
    "racket-doc"
    "typed-racket-doc"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))

