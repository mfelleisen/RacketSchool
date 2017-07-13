#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-thu-aft"]{@bold{Lab} Practice with Hash Langs}

Start with the @resource["ql"]{QL} implementation. You can pick any of
the QL variants as a starting point, but one with at least type
checking will be the most interesting, and the one with
non-S-expression syntax should be within reach.

@exercise["ex:ql:if"]{Add an @racket[if] form for use in guards or
    expressions to compute field values.}

@exercise["ex:ql:text"]{Add a @racket[text] field type, where
    @filepath{gui.rkt} already provides @racket[text-widget].}
