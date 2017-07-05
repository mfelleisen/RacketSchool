#lang scribble/manual

@(require "shared.rkt")
@(require (for-label redex))

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-tue-mor"]{@bold{Lab} The Mystery Languages of Records}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@;common[]

[FILL]: describe mystery languages. (How much to speak and how much to write?)

One syntax may have many semantics. We will give you three mystery
languages that have the same syntax but different semantics, and your
task will be to find programs that tell them apart.

To install the mystery languages, in DrRacket [FILL].
Alternatively, you can run the following command:
@verbatim{
raco pkg install https://github.com/justinpombrio/RacketSchool
}

The three languages are called @tt{Records1}, @tt{Records2}, and
@tt{Records3}. Once you have installed them, you can run them in
Dr. Racket with @tt{#lang Records1}, etc. Here is the syntax for
these languages:

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language Records 
  (p ::= (prog f ... e))

  (f ::= (defun (x x) e))

  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (e + e)
     ;; strings
     s
     (empty? e)
     (e ++ e)
     ;; functions & let
     (function x)
     (e e)
     x
     (let ((x e)) e)
     ;; records
     {(s e) ...}
     (e |@| e))
  
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))
))
@;%

The @tt{#lang}s hide the @tt{(prog ...)} part of the grammar,
though, so your programs can simply have the form @tt{f ... e}. For
example, this program:
@verbatim{
  (defun (f rec)
    (rec @literal|{@}| "a_field"))
  (f {("a_field" 1)})
}
produces the number 1 in all of the languages. [TODO: test].

@; -----------------------------------------------------------------------------
@exercise["ex:analyze"]{

Explore the differences between @tt{Records1}, @tt{Records2},
and @tt{Records3}. Explain their behaviors, and find programs that
support your explanation. These mystery languages differ in how they
treat records, so focus on that when trying to tell them apart.}

@; -----------------------------------------------------------------------------
@exercise["ex:implement"]{

Your next task is to implement these languages. Begin with this
[LINK: basic Redex language], and extend it with the syntax above to
handle records, with the behavior of @tt{Records1}.

Next, extend the language to instead behave like @tt{Records2}.

Finally, if you have time, try @tt{Records3}.}

@; -----------------------------------------------------------------------------
@exercise["ex:mystery-semantics"]{

@bold{SPOILER: do not click on link} until you have finished the first
two exercises.

Now we'll do the opposite. [LINK: here] is a Redex @italic{semantics}
that extends the @tt{basic} language. Without running any
programs, how does it differ from @tt{Records1}? What program
@italic{would} you run to exhibit the difference?}