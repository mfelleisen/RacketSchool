#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-fri-mor"]{@bold{Lab} Designing Metafunctions}

@goals[
@item{developing meta-functions}
@item{discovering Redex patterns}
]

@common[]

@section[#:tag "lfm" #:style 'unnumbered]{Exercises}

@exercise["ex:xxx"]{Design @racket[bv]. The metafunction determines the
bound variables in a @racket[Lambda] expression. A variable @racket[x] is 
@defterm{bound in @racket[e_Lambda]} if @racket[x] occurs in a
@racket[lambda]-parameter list in e_@racket[Lambda].}
