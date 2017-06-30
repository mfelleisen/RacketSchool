#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-mon-mor"]{@bold{Lab} Playing with PCF-value}

@goals[
@item{@racket[define-language]}
@item{@racket[reduction-relation]}
@item{@racket[apply-reduction-relation]}
@item{@racket[traces]}
]

@;common[]

@section[#:tag "lmm" #:style 'unnumbered]{Exercises}

@exercise["ex:pcf"]{Design a variant of PCF that passes its arguments by
value. This task requires: 
@itemlist[

@item{an appropriate definition of evaluation context (focus on the
leftmost-outermost redex)} 

@item{a adaptation of the basic axiom (@tt{beta})}
]
Do you have to modify the definition of the set of values?
Do you have to modify the @tt{eval} function?}

@exercise["ex:pcf+string"]{Add strings to the PCF language (or your
PCF-value language from the preceding exercise if you are comfortable with
the solution). Check in Redex's documentation for the @racket[string]
pattern. Use the syntax @tt{(e ++ e)} to represent the concatenation
operation on strings. 

@bold{Hint} 
@interaction[
(string-append "hello" " " "world")
]
}

@exercise["ex:pcf+err"]{Modify the PCF language (or your PCF-value language
from the preceding exercise if you are comfortable with the solution) so
that the addition of a function to a number or an @tt{if} with a number in
its test position aren't just irreducible but signal an error and terminate
the program execution. 

This task requires: 
@itemlist[

@item{the addition of an error message to the language} 

@item{the addition of basic reduction relations to step from, say, @tt{(1 +
(1 + (λy.x)))} to an error message.}
]
Are you convinced that you have covered all possible stuck states?}


