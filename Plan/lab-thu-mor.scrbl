#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-thu-mor"]{@bold{Lab} Practice with Macros}

@goals[
@item{define syntax functions}
]

@exercise["ex:r1"]{Develop a compile-time function that adds 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(defun (f:id x:id) e:expr)
))
@;%
to Racket where @racket[f] becomes the identifier of a  function,
@racket[x] its argument, and @racket[f] the function body. }

@exercise["ex:r2"]{Develop a compile-time function that implements a
variant of like @racket[if] that reacts only to @racket[#true] and
@racket[#false] in the test position. 

@bold{Note} In a dynamically typed language, it makes some sense to use all
values except for @racket[#false] as equivalents to @racket[#true]. But
some people are offended and this is a first step toward fixing this wart.}

