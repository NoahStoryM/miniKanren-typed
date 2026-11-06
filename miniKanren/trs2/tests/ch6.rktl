;; Examples from The Reasoned Schemer, second edition (MIT Press, 2018),
;; chapter 6.  Copyright (C) 2018 Massachusetts Institute of Technology.
;; Transcribed for use as tests only; see NOTICE.

(displayln "Test code in chapter 6")

(defrel (alwayso)
  (conde
    [succeed]
    [(alwayso)]))

(defrel (nevero)
  (nevero))

(defrel (very-recursiveo)
  (conde
    [(nevero)]
    [(very-recursiveo)]
    [(alwayso)]
    [(very-recursiveo)]
    [(nevero)]))
;; 1-2, 4-13, 16-23, 25.
(check-equal?
 (run 1 q
   (alwayso))
 '(_0))
(check-equal?
 (run 1 q
   (conde
     [succeed]
     [(alwayso)]))
 '(_0))
;; 4: nonterminating: (run* q (alwayso))
;; 5: nonterminating: (run* q (conde (succeed) ((alwayso))))
(check-equal?
 (run 5 q
   (alwayso))
 '(_0 _0 _0 _0 _0))
(check-equal?
 (run 5 q
   (== 'onion q)
   (alwayso))
 '(onion onion onion onion onion))
;; 8: nonterminating: (run 1 q (alwayso) fail)
(check-equal?
 (run 1 q
   (== 'garlic q)
   succeed
   (== 'onion q))
 '())
;; 10: nonterminating: (run 1 q (== 'garlic q) (alwayso) (== 'onion q))
(check-equal?
 (run 1 q
   (conde
     [(== 'garlic q) (alwayso)]
     [(== 'onion q)])
   (== 'onion q))
 '(onion))
;; 12: nonterminating:
;; (run 2 q (conde ((== 'garlic q) (alwayso)) ((== 'onion q))) (== 'onion q))
(check-equal?
 (run 5 q
   (conde
     [(== 'garlic q) (alwayso)]
     [(== 'onion q) (alwayso)])
   (== 'onion q))
 '(onion onion onion onion onion))
;; 16: nonterminating: (run 1 q (nevero))
(check-equal?
 (run 1 q
   fail
   (nevero))
 '())
(check-equal?
 (run 1 q
   (conde
     [succeed]
     [(nevero)]))
 '(_0))
(check-equal?
 (run 1 q
   (conde
     [(nevero)]
     [succeed]))
 '(_0))
;; 20: nonterminating: (run 2 q (conde (succeed) ((nevero))))
;; 21: nonterminating: (run 1 q (conde (succeed) ((nevero))) fail)
(check-equal?
 (run 5 q
   (conde
     [(nevero)]
     [(alwayso)]
     [(nevero)]))
 '(_0 _0 _0 _0 _0))
(check-equal?
 (run 6 q
   (conde
     [(== 'spicy q) (nevero)]
     [(== 'hot q) (nevero)]
     [(== 'apple q) (alwayso)]
     [(== 'cider q) (alwayso)]))
 '(apple cider apple cider apple cider))
;; 25: the book specifies a million identical answers.
(check-equal?
 (run 1000000 q
   (very-recursiveo))
 (make-list 1000000 '_0))
