#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-mon-mor"]{@bold{Lab} Playing with PCF-value}

to understand
@goals[
@item{@racket[define-language]}
@item{@racket[reduction-relation]}
@item{@racket[apply-reduction-relation]}
@item{@racket[traces]}
]

@common[]

@section[#:tag "lma" #:style 'unnumbered]{Exercises}

@exercise["ex:xxx"]{Design @racket[bv]. The metafunction determines the
bound variables in a @racket[Lambda] expression. A variable @racket[x] is 
@defterm{bound in @racket[e_Lambda]} if @racket[x] occurs in a
@racket[lambda]-parameter list in e_@racket[Lambda].}

