#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/microKanren)
          scribble/example
          "utils.rkt")

@(define mk-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/microKanren)))
@(define-syntax-rule (mk-examples body ...)
   (examples #:eval mk-eval body ...))

@title[#:tag "microKanren"]{microKanren}
@defmodule[typed/microKanren #:packages ("miniKanren-typed")]

This module is a Typed Racket rewrite of microKanren (Hemann and
Friedman, @italic{μKanren: A Minimal Functional Core for Relational
Programming}, Scheme Workshop 2013), from the code at
@url{https://github.com/jasonhemann/microKanren}: the core of
@tt{microKanren.scm} and the user-level forms of
@tt{miniKanren-wrappers.scm}.  The core is a handful of functions over
states and streams; everything else is defined on top of them.

The differences from that code are:

@itemlist[

@item{@bold{Types.}  Every definition has a type.}

@item{@bold{Variables.}  A variable is a struct holding its counter,
@racket[(var 0)], rather than the vector @tt{#(0)}, so that variables
are distinguished from the other values of a term.}

@item{@bold{Literals.}  Literals are compared with @racket[equal?]
rather than @racket[eqv?], as in the other modules of this package.}

@item{@bold{Stream operations.}  The representation of streams is the
paper's, but @racket[mplus], @racket[bind], @racket[Zzz], and
@racket[pull] reach it only through a few operations
(@racket[stream-first], @racket[stream-rest], @racket[stream-cons],
and so on), which read a suspension as an element @racket[pause].  The
same operations are used by @racketmodname[typed/miniKanren/trs2].}

]

@section[#:tag "microKanren-types"]{Types}

@deftogether[(@deftype[Literal]
              @deftype[Term])]{
@racket[Literal] is the type of atomic values: booleans, numbers,
characters, byte strings, strings, keywords, symbols, and
@racket['()].  @racket[Term] is the type of all terms: literals,
variables, and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([c Natural]) #:transparent]
              @defproc[(var=? [x1 Var] [x2 Var]) Boolean])]{
Logic variables.  A variable is identified by its counter @racket[c],
and @racket[var=?] compares counters.
}

@deftogether[(@deftype[Substitution]
              @deftype[State])]{
@racket[Substitution] is defined as @racket[(Listof (Pair Var Term))],
a triangular association list.  @racket[State] is defined as
@racket[(Pair Substitution Natural)]: a substitution and the counter of
the next fresh variable.
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

A stream is empty, or an answer followed by a stream (@emph{mature}),
or a suspension that produces a stream when called (@emph{immature}).
Read element by element, a suspension is a @racket[pause] followed by
the stream it produces, so a stream is a lazy list of
@racket[(Maybe a)].
}

@deftogether[(@deftype[Pause]
              @defthing[pause Pause]
              @defproc[(pause? [v Any]) Boolean]
              @deftypeconstr[(Maybe a)])]{
@racket[pause] is the only value of @racket[Pause], the element that
stands for a suspension, and @racket[(Maybe a)] is defined as
@racket[(∪ Pause a)].  It is a union rather than a tagged sum, so the
answers themselves must not be pauses.
}

@deftogether[(@defproc[(stream-empty? [$ (Streamof a)]) Boolean]
              @defproc[(stream-first [$ (∪ (Pair a (Streamof a)) (Suspension a))])
                       (Maybe a)]
              @defproc[(stream-rest [$ (∪ (Pair a (Streamof a)) (Suspension a))])
                       (Streamof a)]
              @defform*[#:literals (pause)
                        [(stream-cons a d)
                         (stream-cons pause d)]]
              @defform[(stream-lazy d)])]{
The only code that knows how @racket[Streamof] is represented.
@racket[stream-empty?] recognizes @racket['()].  The first element of
a mature stream is its answer and the rest is the stream after it; the
first element of an immature stream is @racket[pause] and the rest is
the stream produced by calling the suspension, so @racket[stream-rest]
forces it.  @racket[(stream-cons a d)] is @racket[(cons a d)], and
@racket[(stream-cons pause d)], written with the identifier
@racket[pause] itself, is the suspension @racket[(λ () d)];
@racket[(stream-lazy d)] is @racket[(stream-cons pause d)].
}

@deftype[Goal]{
Defined as @racket[(→ State (Streamof State))].

@mk-examples[
((== 1 1) empty-state)
((call/fresh (λ (q) (== q 5))) empty-state)
]
}

@section[#:tag "microKanren-core"]{The Core}

@defproc[(== [u Term] [v Term]) Goal]{
Unifies @racket[u] and @racket[v] in the substitution of the state.
}

@defproc[(call/fresh [f (→ Var Goal)]) Goal]{
Applies @racket[f] to a new variable, numbered by the counter of the
state, and runs the resulting goal in a state whose counter is
incremented.
}

@deftogether[(@defproc[(disj [g1 Goal] [g2 Goal]) Goal]
              @defproc[(conj [g1 Goal] [g2 Goal]) Goal])]{
Disjunction combines the streams of @racket[g1] and @racket[g2] with
@racket[mplus]; conjunction runs @racket[g2] on every state of
@racket[g1] with @racket[bind].

@mk-examples[
(take-all
 ((conj (call/fresh (λ (a) (== a 7)))
        (call/fresh (λ (b) (disj (== b 5) (== b 6)))))
  empty-state))
]
}

@deftogether[(@defproc[(unit [s/c a]) (Streamof a)]
              @defthing[mzero Null]
              @defproc[(mplus [$1 (Streamof a)] [$2 (Streamof a)]) (Streamof a)]
              @defproc[(bind [$ (Streamof a)] [g (→ a (Streamof b))])
                       (Streamof b)])]{
The stream monad.  @racket[mplus] appends two streams, swapping them
when the first element of the first is a @racket[pause], which is what
makes the search interleave.
}

@deftogether[(@defproc[(walk [u Term] [s Substitution]) Term]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution]
              @defproc[(unify [u Term] [v Term] [s Substitution])
                       (Option Substitution)])]{
Walking, extending a substitution, and unification.  There is no
occurs check.
}

@section[#:tag "microKanren-wrappers"]{User-Level Forms}

These are the forms of @tt{miniKanren-wrappers.scm}.

@defform[(Zzz goal)]{
The inverse-η-delay: a goal that suspends before running
@racket[goal], so that a recursive relation produces an immature
stream instead of looping.  It is defined as
@racket[(λ ([s/c : State]) (stream-lazy (goal s/c)))].
}

@deftogether[(@defform[(conj+ goal ...+)]
              @defform[(disj+ goal ...+)]
              @defform[(fresh (x ...) goal ...+)]
              @defform[(conde [goal ...+] ...+)])]{
N-ary conjunction and disjunction of suspended goals, fresh variables,
and disjunction of conjunctions.

@mk-examples[
(: appendo (→ Term Term Term Goal))
(define (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))
(run* (x y) (appendo x y '(1 2 3)))
]
}

@deftogether[(@defform[(run n (x ...+) goal ...+)
                       #:contracts ([n Natural])]
              @defform[(run* (x ...+) goal ...+)])]{
Runs the goals with fresh variables @racket[x ...] and returns at most
@racket[n] (or all) values of the first variable, reified.
}

@deftogether[(@defthing[empty-state State]
              @defproc[(call/goal [g Goal]) (Streamof State)]
              @defproc[(pull [$ (Streamof a)]) (∪ Null (Pair a (Streamof a)))]
              @defproc[(take [n Natural] [$ (Streamof a)]) (Listof a)]
              @defproc[(take-all [$ (Streamof a)]) (Listof a)])]{
Running goals: @racket[pull] skips the pauses at the front of a
stream, so that it is mature or empty, and @racket[take] and
@racket[take-all] collect its states.
}

@deftogether[(@defproc[(reify-1st [s/c State]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify-name [n Natural]) Symbol])]{
Reification of the value of the first variable, with the names
@racket['_.0], @racket['_.1], and so on.
}

@defproc[(fresh/nf [n Natural] [f (→ Var * Goal)]) Goal]{
Applies @racket[f] to @racket[n] fresh variables.
}
