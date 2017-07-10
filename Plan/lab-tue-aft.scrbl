#lang scribble/manual

@(require "shared.rkt")
@(define basics 
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/public/basic.rkt")

@(define basics+ 
  "https://raw.githubusercontent.com/mfelleisen/RacketSchool/master/Exercises/mystery-semantics-functions.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-tue-aft"]{@bold{Lab} The Mystery Languages of Functions}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@;common[]

One syntax may have many semantics. We will give you three mystery
languages that have the same syntax but different semantics, and your
task will be to find programs that tell them apart.

This lab's mystery languages are called @tt{Functions1},
@tt{Functions2}, and @tt{Functions3}. Run them with @tt{#lang
Functions1}, etc.
For your convenience, @tt{#lang FunctionsAll} will run
programs in @italic{all three} languages.
Here is the syntax for these languages (it is simply
the syntax of the @tt{basic} language; there are no syntactic
extensions):

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
))
@;%

As before, the @tt{#lang}s hide the @tt{(prog ...)} part of the grammar,
so your programs can simply have the form @tt{f ... e}.
For example, this program:
@verbatim{
  (defun (f x) (+ x 1))
  (f 1)
}
produces the number 2 in all of the languages.

@; -----------------------------------------------------------------------------
@exercise["ex:analyze-functions"]{

Explore the differences between @tt{Functions1}, @tt{Functions2},
and @tt{Functions3}. Explain their behaviors, and find programs that
support your explanation. These mystery languages differ in how they
treat function calls, so focus on that when trying to tell them apart.}

@; -----------------------------------------------------------------------------
@exercise["ex:implement-functions"]{

Your task is to develop Redex models for these languages. 

Begin as before with the @hyperlink[basics]{@tt{basic} Redex language}. 
@emph{Modify} it as necessary to model the behavior of @tt{Functions1}.

Next, @emph{modify} the @tt{basic} language to instead behave like @tt{Functions2}.

Finally, if you have time, try @tt{Functions3}.  @bold{Hint} For this last
variant, you must extend the existing syntax so you can express the
run-time states. Like evaluation contexts, for example, this extra syntax
isn't part of the surface syntax.}

@; -----------------------------------------------------------------------------
@exercise["ex:mystery-semantics-functions"]{

Now we will flip the process. Instead of asking you to analyze a language
by writing programs, we would like you to study a model and predict
differences to existing languages. 

@centerline{@bold{SPOILER ALERT: Do not click on link until you have
finished the above two exercises.}}

@hyperlink[basics+]{Here} is a Redex semantics that extends the @tt{basic}
language. @italic{Without running any programs}, how does it differ from
@tt{Functions1}? What programs @italic{would} you run to exhibit the
differences?}
