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

@; -----------------------------------------------------------------------------
@exercise["ex:pcf"]{Design a variant of PCF, including the evaluator, that
passes its arguments by value. This task requires: 
@itemlist[

@item{the use of @tt{beta-z} instead of @tt{beta-z}}

@item{a reflection on the relations for basic arithmetic}

@item{an appropriate definition of evaluation context (focus on the
leftmost-outermost redex)} 

]
Do you have to modify the definition of the set of values?
Do you have to modify the @tt{eval} function?}

@; -----------------------------------------------------------------------------
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

@; -----------------------------------------------------------------------------
@exercise["ex:pcf+err"]{In PCF (as defined), a program may end up adding a
function to a number or let a function flow into the test position of an
@tt{if}. In the existing model, the ``execution'' of such programs gets
@defterm{stuck}. Take a look at this program 
@ntt{(λx.(if x (+ x 1) (+ (λy.y) 2)))}
Suppose the program is applied to @tt{1}. What kind of reduction sequence
do you get in the existing PCF model? What is the result of @racket[eval]? 

Eliminate all stuck states from the PCF model (or your PCF-value language
from the preceding exercise if you are comfortable with your solution).
That is, add a reduction relation that recognizes bad situations, say,
@tt{(1 + (1 + (λy.x)))}, and transitions to an error state.

This task requires: 
@itemlist[

@item{the addition of an error message to the language; and} 

@item{the addition of reduction relations from stuck states to error messages.}
]
Are you convinced that you have covered all possible stuck states?}


