#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-wed-mor"]{@bold{Lab} The Mystery Languages of Variables}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@;common[]

One syntax may have many semantics. We will give you three mystery
languages that have the same syntax but different semantics, and your
task will be to find programs that tell them apart.

This lab's mystery languages are called @tt{Variables1},
@tt{Variables2}, and @tt{Variables3}. Run them with @tt{#lang
Variables1}, etc.
For your convenience, @tt{#lang VariablesAll} will run
programs in @italic{all three} languages.
Here is the syntax for these languages:

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language basic-syntax
  (p ::= (prog f ... e))
  (f ::= (defun (x x) e))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (+ e e)
     ;; strings
     s
     (empty? e)
     (++ e e)
     ;; functions & let
     (function x)
     (e e)
     x
     (let ((x e)) e))
  (x ::= variable-not-otherwise-mentioned)
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (v ::= b n s (function x))
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))

(define-extended-language var-syntax basic-lang
  (f ::= ....
     (defvar x v))
  (e ::= ....
     (set! x e)
     (begin e ...)))
))
@;%

As before, the @tt{#lang}s hide the @tt{(prog ...)} part of the grammar,
so your programs can simply have the form @tt{f ... e}.
For example, this program:
@verbatim{
  (defvar x 1)
  (begin (set! x 2) x)
}
produces the number 2 in all of the languages.

@; -----------------------------------------------------------------------------
@exercise["ex:analyze-variables"]{

Explore the differences between @tt{Variables1}, @tt{Variables2},
and @tt{Variables3}. Explain their behaviors, and find programs that
support your explanation. These mystery languages differ in how they
treat variables and function calls, so focus on that when trying to tell them apart.}

@; -----------------------------------------------------------------------------
@exercise["ex:implement-variables"]{

Your next task is to implement these languages. Begin with this
@hyperlink["https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/public/basic.rkt"]{@tt{basic} Redex language},
and extend it with the syntax above to
handle variables, with the behavior of @tt{Variables1}.

Next, extend the @tt{basic} language to instead behave like @tt{Variables2}.

Finally, if you have time, try @tt{Variables3}.}

@; -----------------------------------------------------------------------------
@exercise["ex:mystery-semantics-variables"]{

@bold{SPOILER: Do not click on link} until you have finished the above
two exercises.

Now we'll do the opposite.
@hyperlink["https://raw.githubusercontent.com/mfelleisen/RacketSchool/master/Exercises/mystery-semantics-variables.rkt"]{Here}
is a Redex semantics
that extends the @tt{basic} language. @italic{Without running any
programs}, how does it differ from @tt{Variables1}? What programs
@italic{would} you run to exhibit the differences?}
