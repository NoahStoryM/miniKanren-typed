#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/miniKanren/trs2)
          scribble/example
          "utils.rkt")

@(define mk-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/miniKanren/trs2)))
@(define-syntax-rule (mk-examples body ...)
   (examples #:eval mk-eval body ...))

@title[#:tag "trs2"]{Second Edition}
@defmodule[typed/miniKanren/trs2 #:packages ("miniKanren-typed")]

This module implements the miniKanren language used in the second
edition of @italic{The Reasoned Schemer} (Friedman, Byrd, Kiselyov, and
Hemann, MIT Press, 2018).  The language is smaller than that of the
first edition: @racket[conde] is the only disjunction and it
interleaves, relations are defined with @racket[defrel], and most of
the implementation consists of ordinary functions whose types can be
read directly.

@section[#:tag "trs2-basis"]{Implementation Basis}

The implementation is a Typed Racket rewrite of the official code of
the second edition,
@url{https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd},
which is the code of Chapter 10 (``Under the Hood'') and the appendix
``Connecting the Wires''.

The differences from that code are:

@itemlist[

@item{@bold{Types.}  Every definition has a type, and @racket[defrel]
supplies the types of the relations it defines (see @racket[defrel]).}

@item{@bold{Variables.}  A variable is a struct compared with
@racket[eq?] and named after the identifier introduced by
@racket[fresh], rather than a vector compared with @racket[eqv?].}

@item{@bold{Literals.}  Literals are compared with @racket[equal?]
rather than @racket[eqv?], so that two equal strings unify even when
they are different objects.}

@item{@bold{Occurs check.}  @racket[ext-s] skips the occurs check when
the value is ground, which cannot create a cycle.  This does not change
the results; it lets the type of @racket[ext-s] say that such an
extension always succeeds.}

@item{@bold{Stream operations.}  The representation of streams is the
book's, but the code of Chapter 10 reaches it only through a few
operations (@racket[stream-first], @racket[stream-rest],
@racket[stream-cons], and so on; see @secref["trs2-streams"]), which
read a suspension as an element @racket[pause].  Each function keeps
the cases and the recursion of the book.}

]

Throughout this section, ``Chapter @italic{n}'' refers to the second
edition.

@section[#:tag "trs2-types"]{Types}

@deftogether[(@deftype[Literal]
              @deftype[Data]
              @deftype[Term])]{
@racket[Literal] is the type of atomic values: booleans, numbers,
characters, byte strings, strings, keywords, symbols, and
@racket['()].  @racket[Data] is the type of ground terms, built from
literals and pairs.  @racket[Term] is the type of all terms: literals,
variables, and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([name Symbol]) #:transparent]
              @defproc[(data? [v Any]) Boolean])]{
Logic variables, compared by identity, and the predicate for
@racket[Data].  (Chapter 10.)
}

@deftype[Substitution]{
Defined as @racket[(Listof (Pair Var Term))], a triangular association
list from variables to terms.
}

@deftogether[(@deftypeconstr[(Streamof a)]
              @deftypeconstr[(Suspension a)])]{
Defined as

@racketblock[
(define-type (Streamof a)
  (∪ Null (Pair a (Streamof a)) (Suspension a)))
(define-type (Suspension a)
  (→ (Streamof a)))
]

A stream is empty, or an answer followed by a stream (a @emph{mature}
stream), or a suspension that produces a stream when it is called (an
@emph{immature} stream).  (Chapter 10.)

The three cases are distinguished by @racket[null?], @racket[pair?],
and @racket[procedure?], none of which inspects an answer, so no
tagging is needed to tell them apart in Typed Racket.  An immature
stream need not be preceded by an answer: a computation can give up
control before it has produced anything.  This is what lets
@racket[conde] switch away from a branch that never produces an answer.

Read element by element, a suspension is a @racket[pause] followed by
the stream it produces, so a stream is a lazy list of
@racket[(Maybe a)]: of answers and pauses.  The operations of
@secref["trs2-streams"] present it in these terms.
}

@deftype[Goal]{
Defined as @racket[(→ Substitution (Streamof Substitution))].

@mk-examples[
((== 1 1) empty-s)
((== 1 2) empty-s)
]
}

@section[#:tag "trs2-interface"]{Running Programs}

@defform*[((run n q goal ...)
           (run n (x ...+) goal ...))
          #:contracts ([n (Option Natural)])]{
Runs the conjunction of the @racket[goal]s and returns a list of at
most @racket[n] reified answers, or of all answers when @racket[n] is
@racket[#f].  In the first form, @racket[q] is bound to a fresh
variable and each answer is its value.  In the second form, each
@racket[x] is bound to a fresh variable and each answer is the list of
their values.  Unassociated variables are reified as @racket['_0],
@racket['_1], and so on.  (Chapter 1.)

@mk-examples[
(run* q (== q '(1 2)))
(run* (x y) (== x y))
(run 0 q succeed)
]
}

@defform[(run* q goal ...)]{
Like @racket[run] with @racket[n] equal to @racket[#f].  It also
accepts the form @racket[(run* (x ...+) goal ...)].
}

@section[#:tag "trs2-goals"]{Goals and Relations}

@deftogether[(@defthing[succeed Goal]
              @defthing[fail Goal])]{
The book's @tt{#s} and @tt{#u}.  (Chapter 1.)
}

@defproc[(== [u Term] [v Term]) Goal]{
Unifies @racket[u] and @racket[v], with the occurs check.  (Chapter 1;
implemented in Chapter 10.)

@mk-examples[
(run* q (fresh (x) (== q (cons x 2)) (== x 1)))
(run* q (== q (list q)))
]
}

@defform[(fresh (x ...) goal ...)]{
Binds each @racket[x] to a new variable, named after the identifier,
and behaves as the conjunction of the @racket[goal]s.  (Chapter 1.)
}

@defform[#:literals (:)
         (defrel (name arg ...) goal ...)
         #:grammar ([arg x [x : type]])]{
Defines @racket[name] as a relation: a function whose result is a
@racket[Goal] that suspends before running the conjunction of the
@racket[goal]s.  An argument written as a plain @racket[x] has type
@racket[Term]; an argument written as @racket[[x : type]] has the given
type, which lets a relation take goals or other relations as
arguments.  For plain arguments, it expands to

@racketblock[
(define (name [x : Term] ...) : Goal
  (λ (s)
    (stream-lazy ((conj goal ...) s))))
]

so the types of the relation are supplied by @racket[defrel] itself.
The @racket[stream-lazy], which is the book's @racket[(λ () ...)], is
the @racket[Suspension] that makes the stream immature; this is the only place where the language suspends a
computation, and therefore the only place where @racket[conde]
switches between branches.  (Chapter 1; implemented in the appendix.)

@mk-examples[
(defrel (twiceo [g : Goal])
  (conde [g] [g]))
(run* q (twiceo (== q 'x)))
]
}

@defform[(conde [goal ...+] ...)]{
Interleaving disjunction of conjunctions.  There is no @racket[else]:
every line has the same form.  (Chapter 1.)

@mk-examples[
(defrel (repeato x l)
  (conde
    [(== l '())]
    [(fresh (d) (== l (cons x d)) (repeato x d))]))
(run 6 q (conde [(repeato 'a q)] [(repeato 'b q)]))
]
}

@deftogether[(@defform[(conda [goal ...+] ...+)]
              @defform[(condu [goal ...+] ...+)])]{
Committed choice: the first goal of each line is a test, and the first
line whose test succeeds is the only one used.  @racket[conda] keeps
every answer of the test; @racket[condu] keeps only the first, by
wrapping each test in @racket[once].  (Chapter 9.)

@mk-examples[
(run* q (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* q (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
]
}

@defform[(project (x ...) goal ...)]{
Rebinds each @racket[x] to its value under the current substitution
(as by @racket[walk*]) and runs the conjunction of the @racket[goal]s.
(Chapter 10.)

@mk-examples[
(run* q
  (fresh (n)
    (conde [(== n 3)] [(== n 4)])
    (project (n)
      (if (exact-integer? n) (== q (* n n)) fail))))
]
}

@deftogether[(@defform[(disj goal ...)]
              @defform[(conj goal ...)])]{
N-ary disjunction and conjunction, built from @racket[disj2] and
@racket[conj2]; @racket[(disj)] is @racket[fail] and @racket[(conj)]
is @racket[succeed].  (Appendix.)
}

@deftogether[(@defproc[(disj2 [g1 Goal] [g2 Goal]) Goal]
              @defproc[(conj2 [g1 Goal] [g2 Goal]) Goal])]{
Binary disjunction and conjunction: @racket[disj2] combines the streams
of @racket[g1] and @racket[g2] with @racket[append-inf];
@racket[conj2] runs @racket[g2] on every answer of @racket[g1] with
@racket[append-map-inf], that is, it is composition in the Kleisli
category of the stream monad.  (Chapter 1; implemented in Chapter 10.)
}

@deftogether[(@defproc[(ifte [g1 Goal] [g2 Goal] [g3 Goal]) Goal]
              @defproc[(once [g Goal]) Goal])]{
@racket[ifte] runs @racket[g2] on every answer of @racket[g1] if
@racket[g1] has an answer, and runs @racket[g3] otherwise; it is the
function behind @racket[conda].  @racket[once] keeps only the first
answer of @racket[g]; it is what turns @racket[conda] into
@racket[condu].  Both force suspensions only as far as needed to find
out whether there is an answer.  (Chapter 10.)
}

@defproc[(call/fresh [name Symbol] [f (→ Var Goal)]) Goal]{
Applies @racket[f] to a new variable named @racket[name]; the function
behind @racket[fresh].  (Chapter 10.)
}

@defproc[(run-goal [n (Option Natural)] [g Goal]) (Listof Substitution)]{
Runs @racket[g] on @racket[empty-s] and returns at most @racket[n] of
the resulting substitutions.  (Chapter 10.)
}

@section[#:tag "trs2-substitutions"]{Substitutions and Unification}

These are the operations of Chapter 10.

@defthing[empty-s Substitution]{
The empty substitution.
}

@defproc*[([(ext-s [x Var] [v Data] [s Substitution]) Substitution]
           [(ext-s [x Var] [v Term] [s Substitution]) (Option Substitution)])]{
Extends @racket[s] with the association of @racket[x] to @racket[v],
or returns @racket[#f] if @racket[x] occurs in @racket[v].  Extending
with a ground term always succeeds, and the type says so.

@mk-examples[
(define x (var 'x))
(ext-s x '(1 2) empty-s)
(ext-s x (list 1 x) empty-s)
]
}

@deftogether[(@defproc[(walk [v Term] [s Substitution]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(occurs? [x Var] [v Term] [s Substitution]) Boolean]
              @defproc[(unify [u Term] [v Term] [s Substitution])
                       (Option Substitution)])]{
@racket[walk] follows the associations of a variable; @racket[walk*]
does so throughout a term; @racket[occurs?] determines whether
@racket[x] occurs in the walked @racket[v]; @racket[unify] returns an
extension of @racket[s] that makes @racket[u] and @racket[v] equal, or
@racket[#f].

@mk-examples[
(define x (var 'x))
(define y (var 'y))
(unify (cons x 2) (cons 1 y) empty-s)
]
}

@deftogether[(@defproc[(reify-name [n Index]) Symbol]
              @defproc[(reify-s [v Term] [r Substitution]) Substitution]
              @defproc[(reify [v Term]) (→ Substitution Term)])]{
Reification.  @racket[reify] is curried: @racket[(reify v)] is the
function that @racket[run] maps over the substitutions it obtains.

@mk-examples[
(define x (var 'x))
(define y (var 'y))
((reify (list x y x)) empty-s)
]
}

@section[#:tag "trs2-streams"]{Streams}

The first group of definitions reads a stream as a lazy list of
answers and pauses; they are the only code that knows how
@racket[Streamof] is represented.  The second group is the stream
operations of Chapter 10, written with the first.  They are polymorphic
in the type of answers, and together they form the stream monad: a
single answer @racket[(stream a)] is the unit,
@racket[empty-stream] is zero, @racket[append-inf] is the sum, and @racket[append-map-inf] is
the bind.

@deftogether[(@deftype[Pause]
              @defthing[pause Pause]
              @defproc[(pause? [v Any]) Boolean]
              @deftypeconstr[(Maybe a)])]{
@racket[pause] is the only value of @racket[Pause], the element that
stands for a suspension.  @racket[(Maybe a)] is defined as
@racket[(∪ Pause a)]: an element of a stream of @racket[a]s.  It is a
union rather than a tagged sum, so the answers themselves must not be
pauses; for answer types that exclude @racket[Pause], such as
@racket[Substitution], Typed Racket checks this.
}

@deftogether[(@defthing[empty-stream Null]
              @defform[(stream a ...)]
              @defproc[(stream-empty? [s-inf (Streamof a)]) Boolean]
              @defproc*[([(stream-first [s-inf (Pair a (Streamof a))]) a]
                         [(stream-first [s-inf (∪ (Pair a (Streamof a)) (Suspension a))])
                          (Maybe a)])]
              @defproc[(stream-rest [s-inf (∪ (Pair a (Streamof a)) (Suspension a))])
                       (Streamof a)])]{
@racket[empty-stream] is the empty stream @racket['()], and
@racket[stream-empty?] recognizes it; @racket[(stream a ...)] is the
mature stream of the answers @racket[a ...].  The
first element of a mature stream is its answer and the rest is the
stream after it; the first element of an immature stream is
@racket[pause] and the rest is the stream produced by calling the
suspension.  So @racket[stream-rest] forces a suspension, and calling
it twice on the same suspension computes the rest twice; the
operations of this section call it once.

@mk-examples[
(define s-inf (stream-lazy (begin (displayln "forced") (stream-cons 1 empty-stream))))
(stream-first s-inf)
(stream-rest s-inf)
]
}

@deftogether[(@defform*[#:literals (pause)
                        [(stream-cons a d)
                         (stream-cons pause d)]]
              @defform[(stream-lazy d)])]{
@racket[(stream-cons a d)] is the answer @racket[a] followed by the
stream @racket[d], that is, @racket[(cons a d)].
@racket[(stream-cons pause d)], written with the identifier
@racket[pause] itself, is a pause followed by @racket[d], that is, the
suspension @racket[(λ () d)]: @racket[d] is evaluated only when the
suspension is forced.  @racket[(stream-lazy d)] is
@racket[(stream-cons pause d)].
}

@defproc[(stream-force [s-inf (Streamof a)]) (∪ Null (Pair a (Streamof a)))]{
Skips the pauses at the front of @racket[s-inf], forcing its
suspensions until the stream is empty or mature.  It does not return
if every element from some point on is a pause.  The operations of
Chapter 10 do not use it: where they meet a pause, they return a new
pause instead, so that another stream can run in the meantime.

@mk-examples[
(stream-force (stream-lazy (stream-lazy (stream-cons 'a empty-stream))))
]
}

@defproc[(append-inf [s-inf (Streamof a)] [t-inf (Streamof a)])
         (Streamof a)]{
Appends two streams.  When @racket[s-inf] is a suspension, the result
is a suspension that, when called, continues with @racket[t-inf] first
and the rest of @racket[s-inf] second: the swap is what makes the
search interleave.  (Chapter 10.)

@mk-examples[
(take-inf #f (append-inf (stream-lazy (stream-cons 1 (stream-cons 2 empty-stream)))
                         (stream-cons 3 (stream-cons 4 empty-stream))))
]
}

@defproc[(append-map-inf [g (→ a (Streamof b))] [s-inf (Streamof a)])
         (Streamof b)]{
Applies @racket[g] to every answer of @racket[s-inf] and combines the
results with @racket[append-inf].  The type is the full monadic bind,
with separate types for the answers before and after.  (Chapter 10.)
}

@defproc[(take-inf [n (Option Natural)] [s-inf (Streamof a)])
         (Listof a)]{
Returns the first @racket[n] answers of @racket[s-inf] (all of them
when @racket[n] is @racket[#f]), calling suspensions as needed.
(Chapter 10.)

@mk-examples[
(take-inf 3 (append-inf (stream-cons 'a empty-stream)
                        (stream-lazy (stream-cons 'b (stream-cons 'c empty-stream)))))
]
}
