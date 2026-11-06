#lang scribble/manual

@(require "editions.rkt" (for-label typed/racket/base))

@title{miniKanren-typed: Typed miniKanren}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

This package provides Typed Racket implementations of the miniKanren
languages of both editions of @italic{The Reasoned Schemer}, one module
per edition:

@itemlist[

@item{@racketmodname[typed/miniKanren/trs1]: the first edition
(Friedman, Byrd, and Kiselyov, MIT Press, 2005), with the depth-first
@trs1[conde] and @trs1[all], the interleaving @trs1[condi] and
@trs1[alli], and the committed-choice @trs1[conda] and
@trs1[condu].}

@item{@racketmodname[typed/miniKanren/trs2]: the second edition
(Friedman, Byrd, Kiselyov, and Hemann, MIT Press, 2018), with
@trs2[defrel] and an interleaving @trs2[conde].}

]

Each implementation follows the code of its edition closely, so that
it can be read alongside the book, and gives every definition a type,
so that the structure of the implementation (terms, substitutions,
goals, and the streams that carry answers) is visible in the types.
Every internal operator is exported.

The two modules export many of the same names, so a program should
require only one of them, or use @racket[prefix-in].

The implementation of the first edition is derived from the code
printed in the book, which is not covered by the package's MIT License;
the implementation of the second edition is derived from the official
code of that edition, which is MIT-licensed.  The file @tt{NOTICE} in
the source repository lists the sources, their copyright holders, and
their licenses.

@table-of-contents[]

@include-section["trs1.scrbl"]
@include-section["trs2.scrbl"]
@include-section["comparison.scrbl"]
