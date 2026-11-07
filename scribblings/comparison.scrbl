#lang scribble/manual

@(require "editions.rkt")

@title[#:tag "comparison"]{Comparing the Editions}

Both editions describe the same relational language at its core:
terms, substitutions, unification with the occurs check (in this
package), and reification are essentially the same, and a program that
uses only @trs1[==], @trs1[fresh], conjunction, disjunction, and
recursion denotes the same relation in both.  The editions differ in
the forms they offer and in how they search.  In this section, code in
the style of @secref["trs1"] links to @racketmodname[typed/miniKanren/trs1],
and code in the style of @secref["trs2"] links to
@racketmodname[typed/miniKanren/trs2].

@section[#:tag "comparison-language"]{The Language}

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{} @bold{first edition} @bold{second edition})
      (list "run"
            @trs1[(run n (q) goal ...)]
            @elem{@trs2[(run n q goal ...)] or @trs2[(run n (x ...) goal ...)]})
      (list "relations"
            @elem{@trs1[define] with a type annotation}
            @trs2[(defrel (name x ...) goal ...)])
      (list "disjunction"
            @elem{@trs1[conde] (depth-first), @trs1[condi] (interleaving)}
            @elem{@trs2[conde] (interleaving)})
      (list "conjunction"
            @elem{@trs1[all], @trs1[alli] (interleaving)}
            @elem{@trs2[conj], implicit in @trs2[fresh] and lines})
      (list "last line"
            @elem{may be @trs1[else]}
            "no else")
      (list "reified names"
            @racket['_.0] @racket['_0]))]

@section[#:tag "comparison-streams"]{Streams}

The first edition has three kinds of streams: empty, one answer, and
an answer followed by a suspension.

@trs1-block[
(define-type (Streamof a)
  (∪ Null (Boxof a) (Pair a (Suspension a))))
]

The second edition also has three, but a suspension stands on its own:

@trs2-block[
(define-type (Streamof a)
  (∪ Null (Pair a (Streamof a)) (Suspension a)))
]

In the first edition, a computation can only give up control after it
has produced an answer; in the second, it can give up control at any
call to a relation defined with @trs2[defrel].  This has two
consequences.  The first is for types: in the first edition, ``one
answer'' and ``an answer and a suspension'' cannot be told apart for
an arbitrary answer type.  The book restricts the allowed answer
values; @racketmodname[typed/miniKanren/trs1] instead wraps a single
answer in an immutable box to support arbitrary answer types (see
@secref["trs1-basis"]).  In the second edition, the three cases are
told apart by the shape of the stream alone.  The second consequence
is for search, and is shown next.

@section[#:tag "comparison-search"]{Search}

A branch that runs forever without producing an answer stops the
first edition's search, even under @trs1[condi], because the
interleaving operators only switch after an answer:

@trs1-block[
(: anyo (→ Goal Goal))
(define (anyo g) (conde [g] [else (anyo g)]))
(define nevero (anyo fail))
(run 1 (q) (condi [nevero] [succeed])) (code:comment "does not terminate")
]

In the second edition, every call to @trs2[nevero] suspends, and
@trs2[append-inf] switches to the other branch at each suspension:

@trs2-examples[
(defrel (nevero) (nevero))
(run 1 q (conde [(nevero)] [succeed]))
]

Interleaving search in the style of the second edition finds every
answer of a pure program; the first edition's @trs1[conde] and
@trs1[all] are depth-first and do not.  Where both editions interleave,
they switch at different points (after each answer in the first,
at each suspension in the second), so the order of answers, and hence
the first @racket[n] answers returned by @trs1[run], are not
guaranteed to agree.

@section[#:tag "comparison-committed"]{Committed Choice}

@trs1[conda] and @trs1[condu] are not relational, so their results can
depend on details that the pure part of the language leaves open.  One
such detail differs between the editions.  In the first edition, the
last line of a form built with @trs1[cond-aux] (such as @trs1[conde]
or @trs1[condu]) is always run as a plain conjunction,
since it is normally an @trs1[else] line; so when the last line of a
@trs1[condu] is not an @trs1[else] line, its test is not committed to
one answer:

@trs1-examples[
(run* (q) (condu [(conde [(== q 1)] [(== q 2)])]))
]

In the second edition, @trs2[condu] wraps every test in
@trs2[once]:

@trs2-examples[
(run* q (condu [(conde [(== q 1)] [(== q 2)])]))
]
