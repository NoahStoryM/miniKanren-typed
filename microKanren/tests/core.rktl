;; Adapted from microKanren-test.scm in
;; https://github.com/jasonhemann/microKanren
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.
;; MIT License; see NOTICE.  Variables, written #(n) there, are (var n).

;; "second-set t1"
(check-equal?
  (let (($ ((call/fresh (λ (q) (== q 5))) empty-state)))
    ($car $))
  `(((,(var 0) . 5)) . 1))

;; "second-set t2"
(check-equal?
  (let (($ ((call/fresh (λ (q) (== q 5))) empty-state)))
    ($cdr $))
  '())

;; "second-set t3"
(check-equal?
  (let (($ (a-and-b empty-state)))
    ($car $))
  `(((,(var 1) . 5) (,(var 0) . 7)) . 2))

;; "second-set t3, take"
(check-equal?
  (let (($ (a-and-b empty-state)))
    (stream->list (stream-take $ 1)))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)))

;; "second-set t4"
(check-equal?
  (let (($ (a-and-b empty-state)))
    ($car ($cdr $)))
  `(((,(var 1) . 6) (,(var 0) . 7)) . 2))

;; "second-set t5"
(check-equal?
  (let (($ (a-and-b empty-state)))
    ($cdr ($cdr $)))
  '())

;; "who cares"
(check-equal?
  (let (($ ((call/fresh (λ (q) (fives q))) empty-state)))
    (stream->list (stream-take $ 1)))
  `((((,(var 0) . 5)) . 1)))

;; "take 2 a-and-b stream"
(check-equal?
  (let (($ (a-and-b empty-state)))
    (stream->list (stream-take $ 2)))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)
    (((,(var 1) . 6) (,(var 0) . 7)) . 2)))

;; "take-all a-and-b stream"
(check-equal?
  (let (($ (a-and-b empty-state)))
    (stream->list $))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)
    (((,(var 1) . 6) (,(var 0) . 7)) . 2)))

;; "ground appendo"
(check-equal?
  ($car ($force (ground-appendo empty-state)))
  `(((,(var 2) b) (,(var 1)) (,(var 0) . a)) . 3))

;; "ground appendo2"
(check-equal?
  ($car ($force (ground-appendo2 empty-state)))
  `(((,(var 2) b) (,(var 1)) (,(var 0) . a)) . 3))

;; "appendo"
(check-equal?
  (stream->list (stream-take (call-appendo empty-state) 2))
  `((((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 3)) (,(var 1))) . 4)
    (((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 6)) (,(var 5)) (,(var 3) ,(var 4) . ,(var 6)) (,(var 1) ,(var 4) . ,(var 5))) . 7)))

;; "appendo2"
(check-equal?
  (stream->list (stream-take (call-appendo2 empty-state) 2))
  `((((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 3)) (,(var 1))) . 4) (((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 3) ,(var 4) . ,(var 6)) (,(var 2) . ,(var 6)) (,(var 5)) (,(var 1) ,(var 4) . ,(var 5))) . 7)))

;; "reify-1st across appendo"
(check-equal?
  (map reify-1st (stream->list (stream-take (call-appendo empty-state) 2)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; "reify-1st across appendo2"
(check-equal?
  (map reify-1st (stream->list (stream-take (call-appendo2 empty-state) 2)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; "many non-ans"
(check-equal?
  (stream->list (stream-take (many-non-ans empty-state) 1))
  `((((,(var 0) . 3)) . 1)))
