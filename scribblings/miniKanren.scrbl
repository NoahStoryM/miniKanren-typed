#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/miniKanren)
          scribble/example)

@(define mk-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/miniKanren)))
@(define-syntax-rule (mk-examples body ...)
   (examples #:eval mk-eval body ...))

@title{miniKanren-typed: Typed miniKanren}
@defmodule[typed/miniKanren #:packages ("miniKanren-typed")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

This library is a Typed Racket implementation of the miniKanren
language used in the @emph{first} edition of @italic{The Reasoned
Schemer} (Friedman, Byrd, and Kiselyov, MIT Press, 2005).  It keeps
the operators of that edition, including the interleaving forms
@racket[condi] and @racket[alli] and the committed-choice forms
@racket[conda] and @racket[condu], which later miniKanren
implementations (and the second edition of the book) have dropped or
redesigned.

The goal of the library is to make the structure of the implementation
visible through its types: terms, substitutions, goals, and the lazy
streams that carry answers all have names, and every internal operator
is exported so that it can be inspected and tested in isolation.

@section[#:tag "basis"]{Implementation Basis}

The implementation follows the one described in the first edition:
unification and reification are those of Chapter 9 (``Under the
Hood''), and the stream operators and goal constructors are those of
the appendix ``Connecting the Wires''.  The second edition is a
different language (its @racket[conde] interleaves, there is no
@racket[condi] or @racket[alli]) and is not what this library
implements.

The differences from the first edition are:

@itemlist[

@item{@bold{Stream representation.}  The book represents ``exactly
one answer'' by the answer itself and ``an answer followed by more''
by a pair whose @racket[cdr] is a procedure.  That union cannot be
discriminated by occurrence typing for an arbitrary answer type, so
here a single answer is wrapped in an immutable box and the empty
stream is @racket['()].  See @racket[Streamof].}

@item{@bold{Occurs check.}  The book's @racket[ext-s] never checks
for circularity; a separate checked variant is introduced in Chapter 9.
Here @racket[ext-s] always performs the occurs check (skipping it when
the value contains no variables), so @racket[==] is the
@emph{checked} unification and there is no separate checked variant.}

@item{@bold{Spelling.}  @racket[succeed] and @racket[fail] stand for
the book's @tt{#s} and @tt{#u}; @racket[λG] and @racket[λF] are typed
versions of the book's goal and thunk lambdas.  Superscripts become
suffixes: @racket[conde], @racket[condi], @racket[alli], @racket[ife],
and so on.}

@item{@bold{Omissions.}  The helper forms that the book introduces in
passing to explain a point (such as the bounded lambda of Chapter 5 and
@tt{project} of Chapter 9) are not part of the library.}

]

Throughout this manual, ``Chapter @italic{n}'' refers to the first
edition.

@section[#:tag "types"]{Types}

@defidform[Literal]{
The type of atomic values that may appear in a term: booleans,
numbers, characters, byte strings, strings, keywords, symbols, and
@racket['()].  Unification compares literals with @racket[equal?].
}

@defidform[Data]{
The type of ground terms: literals and pairs of ground terms, with no
logic variables anywhere inside.
}

@defidform[Term]{
The type of terms: literals, logic variables (@racket[Var]), and pairs
of terms.  Every argument of @racket[==] is a @racket[Term].
}

@deftogether[(@defidform[Var]
              @defproc[(var [name Symbol]) Var]
              @defproc[(var? [v Any]) Boolean]
              @defproc[(var-name [x Var]) Symbol])]{
Logic variables.  A variable is identified by the object itself
(@racket[eq?]), not by its name; the name is only for printing.  Two
calls to @racket[var] with the same name produce distinct variables.

@mk-examples[
(define x (var 'x))
(eq? x x)
(eq? (var 'y) (var 'y))
]
}

@defproc[(data? [v Any]) Boolean]{
Recognizes @racket[Data], that is, terms that contain no variables.
}

@defidform[Substitution]{
Defined as @racket[(Listof (Pair Var Term))].  A substitution is a
triangular association list: a variable may be associated with a term
that itself contains variables bound further along the list, and
@racket[walk] follows those links.
}

@defform[(Streamof a)]{
The type of lazy streams of answers, defined as

@racketblock[
(∪ Null
   (Boxof a)
   (Pair a (→ (Streamof a))))
]

A stream is empty, contains exactly one answer, or contains one answer
followed by a suspended computation (a thunk) that produces the rest.
The middle case is the book's @emph{unit}; the last is its
@emph{choice}.
}

@defidform[Goal]{
Defined as @racket[(→ Substitution (Streamof Substitution))].  A goal
maps a substitution to the stream of all extensions of it under which
the goal holds.  A goal that fails returns the empty stream.

@mk-examples[
((== 1 1) empty-s)
((== 1 2) empty-s)
]
}

@section[#:tag "interface"]{Running Programs}

@deftogether[(@defform[(run n (x) goal ...)
                       #:contracts ([n (Option Integer)])]
              @defform[(run* (x) goal ...)])]{
Introduces a fresh variable @racket[x], runs the conjunction of the
@racket[goal]s from the empty substitution, and returns a list of at
most @racket[n] values of @racket[x], each fully @racket[reify]d.
When @racket[n] is @racket[#f], all values are returned, and the
computation diverges if there are infinitely many.  @racket[run*] is
@racket[(run #f (x) goal ...)].  (Chapter 1.)

Unassociated variables in an answer are reified as the symbols
@racket['_.0], @racket['_.1], and so on, in order of first appearance.

@mk-examples[
(run* (q) (== q '(1 2)))
(run* (q) (fresh (a b) (== q (list a b a))))
(run 0 (q) succeed)
]
}

@section[#:tag "goals"]{Goals}

@deftogether[(@defthing[succeed Goal]
              @defthing[fail Goal])]{
The goal that always succeeds once and the goal that never succeeds.
(Chapter 1.)

@mk-examples[
(run* (q) succeed)
(run* (q) fail)
]
}

@defproc[(== [u Term] [v Term]) Goal]{
Unifies @racket[u] with @racket[v] (see @racket[unify]).  The goal
succeeds once if they unify and fails otherwise.  Because
@racket[ext-s] performs the occurs check, a variable cannot be unified
with a term that properly contains it.  (Chapter 1; the occurs check is
discussed in Chapter 9.)

@mk-examples[
(run* (q) (fresh (x) (== q (cons x 2)) (== x 1)))
(run* (q) (== q (list q)))
]
}

@defform[(fresh (x ...) goal ...)]{
Binds each @racket[x] to a new variable and behaves as
@racket[(all goal ...)].  The variables are created when the resulting
goal is applied to a substitution, not when the @racket[fresh]
expression is evaluated.  (Chapter 1.)

@mk-examples[
(run* (q) (fresh (a d) (== (cons a d) q) (== a 'head)))
]
}

@deftogether[(@defform[(all goal ...)]
              @defform[(alli goal ...)])]{
Conjunction.  @racket[(all)] and @racket[(alli)] are
@racket[succeed]; with goals, the result succeeds for each way of
satisfying every goal in sequence.

@racket[all] explores the answers of the first goal one at a time: for
the first answer it exhausts the rest of the conjunction before moving
to the second answer.  @racket[alli] (``all, interleaved'') alternates
between the answers produced for different answers of the earlier
goals, so a later answer of the first goal is reached even when an
earlier one leads to infinitely many results.  (@racket[all],
Chapter 6; @racket[alli], Chapter 6.)

In the examples below, the first goal chooses @racket['L] or
@racket['R], and @racket[counto] then relates @racket[n] to a list of
@racket['I]s of any length.  The first choice already has infinitely
many answers, so @racket[all] never reaches @racket['R];
@racket[alli] alternates between the two choices.

@mk-examples[
(: counto (→ Term Goal))
(define (counto n)
  (conde
    [(== n '())]
    [(fresh (m) (== n (cons 'I m)) (counto m))]))
(run 6 (q)
  (fresh (n side)
    (all (conde [(== side 'L)] [(== side 'R)]) (counto n))
    (== q (cons side n))))
(run 6 (q)
  (fresh (n side)
    (alli (conde [(== side 'L)] [(== side 'R)]) (counto n))
    (== q (cons side n))))
]
}

@deftogether[(@defform[#:literals (else)
                       (conde clause ...)
                       #:grammar ([clause [goal ...+]
                                          [else goal ...]])]
              @defform[(condi clause ...)])]{
Disjunction.  Each @racket[clause] is a conjunction of goals; the
result succeeds for every answer of every clause.  An @racket[else]
clause, if present, must come last, and behaves like any other clause.
With no clauses the result is @racket[fail].

@racket[conde] is depth-first: it produces all the answers of the first
clause before any answer of the second, so a clause with infinitely
many answers hides every clause after it.  @racket[condi] (``cond,
interleaved'') alternates between the clauses.  (@racket[conde],
Chapter 1; @racket[condi], Chapter 6.)

@mk-examples[
(: repeato (→ Term Term Goal))
(define (repeato x l)
  (conde
    [(== l '())]
    [(fresh (d) (== l (cons x d)) (repeato x d))]))
(run 6 (q) (conde [(repeato 'a q)] [(repeato 'b q)]))
(run 6 (q) (condi [(repeato 'a q)] [(repeato 'b q)]))
]
}

@deftogether[(@defform[(conda clause ...)]
              @defform[(condu clause ...)])]{
Committed choice, with the same clause syntax as @racket[conde].  The
first goal of each clause acts as a test.  The first clause whose test
succeeds is chosen, and the remaining clauses are discarded.

In @racket[conda] (``soft cut''), every answer of the chosen test is
passed to the rest of its clause.  In @racket[condu] (``committed
choice''), only the first answer of the test is kept.  Neither form is
relational in general: the result can depend on how much is already
known about the variables.  (Chapter 10.)

@mk-examples[
(run* (q) (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* (q) (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* (q) (conda [fail] [(== q 3)]))
]
}

@section[#:tag "substitutions"]{Substitutions and Unification}

These are the operations of Chapter 9.

@deftogether[(@defthing[empty-s Substitution]
              @defproc[(size-s [s Substitution]) Index])]{
The empty substitution and the number of associations in a
substitution.
}

@defproc*[([(ext-s [x Var] [v Data] [s Substitution]) Substitution]
           [(ext-s [x Var] [v Term] [s Substitution]) (Option Substitution)])]{
Extends @racket[s] with the association of @racket[x] to @racket[v].
If @racket[v] is ground the extension always succeeds; otherwise
@racket[ext-s] returns @racket[#f] when @racket[x] occurs in
@racket[v] (after walking), which would make the substitution
circular.  The two cases are reflected in the type.

@mk-examples[
(define x (var 'x))
(ext-s x '(1 2) empty-s)
(ext-s x (list 1 x) empty-s)
]
}

@defproc[(occurs? [x Var] [v Term] [s Substitution]) Boolean]{
Determines whether @racket[x] appears in @racket[v] once every variable
in @racket[v] has been replaced through @racket[s].
}

@defproc[(walk [v Term] [s Substitution]) Term]{
If @racket[v] is a variable associated in @racket[s], follows the
chain of associations and returns the first term that is not an
associated variable; otherwise returns @racket[v].  Pairs are not
traversed.

@mk-examples[
(define x (var 'x))
(define y (var 'y))
(walk x (list (cons x y) (cons y 'end)))
(walk (cons x x) (list (cons x 1)))
]
}

@defproc[(walk* [v Term] [s Substitution]) Term]{
Like @racket[walk], but also walks inside pairs, so the result contains
only variables that @racket[s] leaves unassociated.

@mk-examples[
(define x (var 'x))
(walk* (cons x x) (list (cons x 1)))
]
}

@defproc[(unify [u Term] [v Term] [s Substitution]) (Option Substitution)]{
Returns a substitution that extends @racket[s] and makes @racket[u]
and @racket[v] equal, or @racket[#f] if there is none.  Variables are
compared with @racket[eq?], pairs are unified component by component,
and literals are compared with @racket[equal?].

@mk-examples[
(define x (var 'x))
(define y (var 'y))
(unify (cons x 2) (cons 1 y) empty-s)
(unify (cons x 2) (cons 1 3) empty-s)
]
}

@deftogether[(@defproc[(reify-name [n Index]) Symbol]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify [v Term]) Term])]{
Reification turns the unassociated variables in an answer into
readable symbols.  @racket[reify-name] produces the symbol for the
@racket[n]th variable, @racket[reify-s] extends @racket[s] by
associating each not-yet-associated variable in @racket[v] with the
next name, and @racket[reify] applies @racket[walk*] to @racket[v]
with the result of @racket[(reify-s v empty-s)].

@mk-examples[
(define x (var 'x))
(define y (var 'y))
(reify (list x y x))
]
}

@section[#:tag "streams"]{Streams}

These are the operators of the appendix ``Connecting the Wires''.
They are exported so that goals can be written and examined at the
level of streams.

@deftogether[(@defform[(λG (s) body ...+)]
              @defform[#:literals (:) (λF : a body ...+)])]{
@racket[λG] builds a @racket[Goal] whose substitution argument is
@racket[s].  @racket[λF] builds a thunk of type
@racket[(→ (Streamof a))], used to suspend the rest of a stream.
}

@deftogether[(@defform[(mzero)]
              @defform[(unit a)]
              @defform[(choice a f)])]{
Constructors for the three cases of @racket[Streamof]: the empty
stream, the stream of exactly @racket[a], and the stream of
@racket[a] followed by the stream produced by the thunk @racket[f].
}

@defform[#:id case∞
         (case∞ e
           [() on-zero]
           [(a) on-one]
           [(a f) on-choice])]{
Evaluates @racket[e] to a stream and dispatches on its shape, binding
the single answer, or the first answer and the suspended rest.
}

@defproc[(map∞ [n (Option Integer)] [p (→ a b)] [a∞ (Streamof a)])
         (Listof b)]{
Applies @racket[p] to the first @racket[n] answers of @racket[a∞]
(all of them when @racket[n] is @racket[#f]), forcing only as much of
the stream as needed.  This is what @racket[run] uses.

@mk-examples[
(map∞ #f size-s
      ((conde [succeed] [(fresh (x) (== x 1))]) empty-s))
]
}

@deftogether[(@defproc[(mplus [a∞ (Streamof a)] [f (→ (Streamof a))])
                       (Streamof a)]
              @defproc[(bind [a∞ (Streamof a)] [g (→ a (Streamof a))])
                       (Streamof a)])]{
@racket[mplus] appends the stream produced by @racket[f] after
@racket[a∞]; @racket[bind] applies @racket[g] to every answer of
@racket[a∞] and combines the results with @racket[mplus].  Together
with @racket[unit] and @racket[mzero] they form the stream monad on
which @racket[conde] and @racket[all] are built.
}

@deftogether[(@defproc[(mplusi [a∞ (Streamof a)] [f (→ (Streamof a))])
                       (Streamof a)]
              @defproc[(bindi [a∞ (Streamof a)] [g (→ a (Streamof a))])
                       (Streamof a)])]{
Interleaving versions of @racket[mplus] and @racket[bind]: after each
answer, @racket[mplusi] swaps the roles of its two streams.  They
underlie @racket[condi] and @racket[alli].
}

@deftogether[(@defform[(ife g0 g1 g2)]
              @defform[(ifi g0 g1 g2)]
              @defform[(ifa g0 g1 g2)]
              @defform[(ifu g0 g1 g2)])]{
Two-way choices from which the @racket[cond] forms are built:
@racket[(cond-aux ifer clause ...)] turns each clause into
@racket[(ifer test (all goal ...) rest)].  @racket[ife] and
@racket[ifi] behave like @racket[(conde [g0 g1] [g2])] and
@racket[(condi [g0 g1] [g2])].  @racket[ifa] and @racket[ifu] run
@racket[g2] only when @racket[g0] has no answer; otherwise they pass
all answers (@racket[ifa]) or the first answer (@racket[ifu]) of
@racket[g0] to @racket[g1].
}

@deftogether[(@defform[(cond-aux ifer clause ...)]
              @defform[(all-aux bnd goal ...)])]{
The macros that generate the @racket[cond] forms from an
@racket[if]-form and the conjunctions from a binding operator
(@racket[bind] or @racket[bindi]).
}
