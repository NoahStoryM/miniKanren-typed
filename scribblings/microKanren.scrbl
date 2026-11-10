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
paper's, but @racket[mplus], @racket[bind], and @racket[Zzz] reach it
only through a few operations (@racket[stream-first],
@racket[stream-rest], @racket[stream-cons], and so on), which read a
suspension as an element @racket[pause].  The same operations are used
by @racketmodname[typed/miniKanren/trs2].  The paper's @tt{mzero} and
@tt{unit} are @racket[empty-stream] and @racket[stream], and its
@tt{pull}, @tt{take}, and @tt{take-all} are @racket[stream-force],
@racket[stream-force-take], and @racket[stream->list], named after
@racketmodname[racket/stream].}

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

@deftype[Goal]{
Defined as @racket[(→ State (Streamof State))].

@mk-examples[
((== 1 1) empty-state)
((call/fresh (λ (q) (== q 5))) empty-state)
]
}

@section[#:tag "microKanren-streams"]{Streams}

The definitions of this section are the only code that knows how
@racket[Streamof] is represented.  They read a stream element by
element, a suspension being the element @racket[pause]; the rest of the
code, @racket[mplus] and @racket[bind] included, is written with them.

@subsection[#:tag "microKanren-pauses"]{Pauses}

@deftogether[(@deftype[Pause]
              @defthing[pause Pause]
              @defproc[(pause? [v Any]) Boolean]
              @deftypeconstr[(Maybe a)])]{
@racket[pause] is the only value of @racket[Pause], the element that
stands for a suspension, and @racket[(Maybe a)] is defined as
@racket[(∪ Pause a)].  It is a union rather than a tagged sum, so the
answers themselves must not be pauses.
}

@subsection[#:tag "microKanren-stream-constructors"]{Constructing Streams}

@deftogether[(@defthing[empty-stream Null]
              @defform[(stream a ...)]
              @defform*[#:literals (pause)
                        [(stream-cons a d)
                         (stream-cons pause d)]]
              @defform[(stream-lazy d)])]{
@racket[empty-stream] is @racket['()], the paper's @tt{mzero}, and
@racket[(stream a ...)] is the mature stream of the states
@racket[a ...], the paper's @tt{unit} when there is one.
@racket[(stream-cons a d)] is @racket[(cons a d)], and
@racket[(stream-cons pause d)], written with the identifier
@racket[pause] itself, is the suspension @racket[(λ () d)]: @racket[d]
is evaluated only when the suspension is forced.
@racket[(stream-lazy d)] is @racket[(stream-cons pause d)].
}

@subsection[#:tag "microKanren-stream-observers"]{Reading Streams}

@deftogether[(@defproc[(stream-empty? [$ (Streamof a)]) Boolean]
              @defproc*[([(stream-first [$ (Pair a (Streamof a))]) a]
                         [(stream-first [$ (∪ (Pair a (Streamof a)) (Suspension a))])
                          (Maybe a)])]
              @defproc[(stream-rest [$ (∪ (Pair a (Streamof a)) (Suspension a))])
                       (Streamof a)]
              @defproc[(stream-take [$ (Streamof a)] [n Natural]) (Streamof a)])]{
@racket[stream-empty?] recognizes @racket[empty-stream].  The first
element of a mature stream is its answer and the rest is the stream
after it; the first element of an immature stream is @racket[pause] and
the rest is the stream produced by calling the suspension, so
@racket[stream-rest] forces it.  @racket[(stream-take $ n)] is the
stream of the first @racket[n] elements of @racket[$], pauses included.
}

@subsection[#:tag "microKanren-stream-forcing"]{Forcing Streams}

@deftogether[(@defproc[(stream-force [$ (Streamof a)]) (∪ Null (Pair a (Streamof a)))]
              @defproc[(stream-force-take [$ (Streamof a)] [n Natural]) (Listof a)]
              @defproc[(stream->list [$ (Streamof a)]) (Listof a)])]{
These force pauses instead of returning them, so they count and
collect only states.  @racket[stream-force] skips the pauses at the
front of a stream, so that it is empty or mature; it is the paper's
@tt{pull}.  @racket[(stream-force-take $ n)] is the list of the first
@racket[n] states of @racket[$], the paper's @tt{(take n $)}, and
@racket[stream->list] is the list of all of them, the paper's
@tt{(take-all $)}.  They do not return if a stream pauses forever
before it has as many states as they need.

@mk-examples[
(define $ (stream-lazy (stream 1 2)))
(stream-force $)
(code:comment "the pause is the first element")
(stream->list (stream-take $ 2))
(stream-force-take $ 1)
(stream->list $)
]
}

@subsection[#:tag "microKanren-monad"]{The Stream Monad}

@deftogether[(@defproc[(mplus [$1 (Streamof a)] [$2 (Streamof a)]) (Streamof a)]
              @defproc[(bind [$ (Streamof a)] [g (→ a (Streamof b))])
                       (Streamof b)])]{
The stream monad, with @racket[stream] as unit and @racket[empty-stream]
as zero.  @racket[mplus] appends two streams, swapping them when the
first element of the first is a @racket[pause], which is what makes
the search interleave; @racket[bind] runs @racket[g] on every state of
@racket[$] and combines the results with @racket[mplus].
}

@section[#:tag "microKanren-core"]{The Core}

The core of @tt{microKanren.scm}: the four goal constructors, and the
unification they are built on.

@subsection[#:tag "microKanren-goals"]{Goal Constructors}

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
(stream->list
 ((conj (call/fresh (λ (a) (== a 7)))
        (call/fresh (λ (b) (disj (== b 5) (== b 6)))))
  empty-state))
]
}

@subsection[#:tag "microKanren-unification"]{Unification}

@deftogether[(@defproc[(walk [u Term] [s Substitution]) Term]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution]
              @defproc[(unify [u Term] [v Term] [s Substitution])
                       (Option Substitution)])]{
Walking, extending a substitution, and unification.  There is no
occurs check.
}

@section[#:tag "microKanren-wrappers"]{User-Level Forms}

These are the forms of @tt{miniKanren-wrappers.scm}.

@subsection[#:tag "microKanren-goal-forms"]{Goal Forms}

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

@defproc[(fresh/nf [n Natural] [f (→ Var * Goal)]) Goal]{
Applies @racket[f] to @racket[n] fresh variables.
}

@subsection[#:tag "microKanren-running"]{Running Goals}

@deftogether[(@defform[(run n (x ...+) goal ...+)
                       #:contracts ([n Natural])]
              @defform[(run* (x ...+) goal ...+)])]{
Runs the goals with fresh variables @racket[x ...] and returns at most
@racket[n] (or all) values of the first variable, reified.
}

@deftogether[(@defthing[empty-state State]
              @defproc[(call/goal [g Goal]) (Streamof State)])]{
Running goals: @racket[call/goal] runs a goal in @racket[empty-state].
@racket[run] collects its states with @racket[stream-force-take], and
@racket[run*] with @racket[stream->list].
}

@subsection[#:tag "microKanren-reification"]{Reification}

@deftogether[(@defproc[(reify-1st [s/c State]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify-name [n Natural]) Symbol])]{
Reification of the value of the first variable, with the names
@racket['_.0], @racket['_.1], and so on.
}
