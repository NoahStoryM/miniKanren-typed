#lang info

(define license 'MIT)
(define collection "typed")
(define version "0.0")

(define pkg-desc "Typed miniKanren")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("rackunit-typed"))
#;
(define scribblings '(("scribblings/miniKanren.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))

