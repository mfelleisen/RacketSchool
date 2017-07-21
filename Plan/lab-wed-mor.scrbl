#lang scribble/manual

@(require "shared.rkt")

@(define basics 
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/public/basic.rkt")

@(define basics+
  "https://raw.githubusercontent.com/mfelleisen/RacketSchool/master/Exercises/mystery-semantics-variables.rkt")

@(define event-loop
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/private/event-loop.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-wed-mor"]{@bold{Lab} The Mystery Languages of Variables}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@bold{Note} While we did not run this lab---replacing it with
@secref{lab-wed-event} instead---we keep it with these notes for readers
who wish to study different flavors of imperative languages. 

One syntax may have many semantics. We will give you three mystery
languages that have the same syntax but different semantics, and your
task will be to find programs that tell them apart.

This lab's mystery languages are called @tt{Variables1},
@tt{Variables2}, and @tt{Variables3}. Run them with @tt{#lang
RacketSchool/Variables1}, etc.
For your convenience, @tt{#lang RacketSchool/VariablesAll} will run
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
@section[#:tag "lwm" #:style 'unnumbered]{Exercises}

@exercise["ex:analyze-variables"]{

Explore the differences between @tt{Variables1}, @tt{Variables2},
and @tt{Variables3}. Explain their behaviors, and find programs that
support your explanation. These mystery languages differ in how they
treat variables and function calls, so focus on that when trying to tell them apart.}

@; -----------------------------------------------------------------------------
@exercise["ex:implement-variables"]{

Your task is to develop Redex models for these languages. 

Begin with this @hyperlink[basics]{@tt{basic} Redex language}. @emph{Extend} it 
with the syntax to handle variables. Then add reduction rules to realize
the behavior of @tt{Variables1}. 

Next, @emph{extend} the @tt{basic} language again to realize the behavior
of @tt{Variables2}. 

Finally, if you have time, try @tt{Variables3}.}

@; -----------------------------------------------------------------------------
@exercise["ex:mystery-semantics-variables"]{

Now we will flip the process. Instead of asking you to analyze a language
by writing programs, we would like you to study a model and predict
differences to existing languages. 

@centerline{@bold{SPOILER ALERT: Do not click on link until you have
finished the above two exercises.}}

@hyperlink[basics+]{Here} is a Redex semantics that extends the @tt{basic}
language. @italic{Without running any programs}, how does it differ from
@tt{Variables1}? What programs @italic{would} you run to exhibit the
differences?}
