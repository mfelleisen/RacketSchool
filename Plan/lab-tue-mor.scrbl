#lang scribble/manual

@(require "shared.rkt")
@(require (for-label redex))

@(define basics 
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/public/basic.rkt")

@(define basics-plus 
  "https://raw.githubusercontent.com/mfelleisen/RacketSchool/master/Exercises/mystery-semantics-records.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-tue-mor"]{@bold{Lab} The Mystery Languages of Records}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@;common[]

One syntax may have many semantics. We will give you three mystery
languages that have the same syntax but different semantics, and your
task will be to find programs that tell them apart.

To install the mystery languages, in DrRacket select
File/InstallPackage, and enter
@tt{racket-school-mystery-languages}.
Alternatively, you can run the following command from the terminal:
@verbatim{
raco pkg install racket-school-mystery-languages
}

The three languages are called @tt{Records1}, @tt{Records2}, and
@tt{Records3}. Once you have installed them, you can run them in
DrRacket with @tt{#lang RacketSchool/Records1}, etc.
For your convenience, @tt{#lang RacketSchool/RecordsAll} will run
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

(define-extended-language record-syntax basic-lang
  (e ::= ....
     (record (s e) ...)
     (|@| e e))
  (v ::= ....
     (record (s v) ...)))
))
@;%

The @tt{#lang}s hide the @tt{(prog ...)} part of the grammar,
though, so your programs can simply have the form @tt{f ... e}. For
example, this program:
@verbatim{
  (defun (f rec)
    (@literal|{@}| rec "a_field"))
  (f (record ("a_field" 1)))
}
produces the number 1 in all of the languages.

@; -----------------------------------------------------------------------------
@section[#:tag "ltum" #:style 'unnumbered]{Exercises}

@exercise["ex:analyze-records"]{
Explore the differences between @tt{Records1}, @tt{Records2}, and
@tt{Records3}. Explain their behaviors, and find programs that support your
explanation. These mystery languages differ in how they treat records, so
focus on that when trying to tell them apart.}

@; -----------------------------------------------------------------------------
@exercise["ex:implement-records"]{

Your task is to develop Redex models for these languages. 

Begin with this @hyperlink[basics]{@tt{basic} Redex language}, and
@emph{extend} it with the syntax above to handle records, with the behavior
of @tt{Records1}.  Instead of producing @tt{'stuck}, however, your language
can just get stuck. For example, @tt{(prog (+ 1))} should evaluate to
@tt{(prog (+ 1))}, rather than to @tt{'stuck}.

Next, extend the @tt{basic} language again to instead behave like @tt{Records2}.

Finally, if you have time, try to model @tt{Records3}.}

@; -----------------------------------------------------------------------------
@exercise["ex:mystery-semantics-records"]{

Now we will flip the process. Instead of asking you to analyze a language
by writing programs, we would like you to study a model and predict
differences to existing languages. 

@centerline{@bold{SPOILER ALERT: Do not click on link until you have finished the above two exercises.}}

@hyperlink[basics-plus]{Here} are two Redex semantics that extends the
@tt{basic} language. @italic{Without running any programs}, how do they
differ from @tt{Records1}? What programs @italic{would} you run to exhibit
the differences?}
