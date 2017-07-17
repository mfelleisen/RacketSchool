#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-thu-mor"]{@bold{Lab} Practice with Macros}

@goals[
@item{define syntax functions}
]

@exercise["mac-or"]{Develop the compile-time function @racket[disjunction2]
(without using @racket[or]). The function deals with binary disjunctions of
the shape 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(disjunction2 lhs:expr rhs:expr)
))
@;%
After testing, run the expression @racket[(disjunction2 (displayln 1) (displayln 2))].

@centerline{@bold{Stop}---before anyone continues, let's discuss}

Now generalize the function to @racket[disjunction], which deals with at
least two but possibly an arbitrary number of expressions.}

@exercise["mac-let*"]{Develop the compile-time function @racket[block]
(without using @racket[let*]). The function implements syntax of the shape 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(block ((x1:id rhs1:expr) (x2:id rhs2:expr) ... (x3:id rhs3:expr)) body:expr)
))
@;%
The identifier @racket[x1] is visible in @racket[rhs2], but not vice
versa, and so on. All @racket[x1] thru @racket[x3] are visible in
@racket[body]. 

The evaluation initializes @racket[x1] to @racket[rhs1], @racket[x2] to
@racket[rhs2], and so on. Once all identifiers are initialized,
@racket[body] is evaluated.}

@exercise["mac-for-loop"]{Develop the compile-time function
@racket[for-loop]. It realizes the following syntax trees as bounded loops
over integers: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(for-loop ([i e0 limit]) body)
))
@;%
or
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(for-loop ([i e0 limit stride]) body)
))
@;%

First, classify each piece of the input tree. 

Second, develop the implementation for the second version @emph{only}.

Third, add the first variant.

Fourth, explore what kind of error messages your implementation yields if
the input tree does not match either pattern.}

@exercise["mac-while"]{Develop the compile-time function @racket[while],
which adds while-do loops to Racket. Create your favorite syntax.}

@exercise["mac-dispatch"]{Develop the compile-time function
@racket[dispatch]. It translates syntax trees of this shape
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(dispatch e
  (s1 e1)
  ...
  (s2 e2)
  (default e-def))
))
@;%
into code that evaluates the expression @tt{e}. If it is not a symbol,
@tt{e-def} is evaluated. If it is a symbol and if this symbol is any of
@tt{s1} ... @tt{s2}, the corresponding expression @tt{e1} ... @tt{e2} is
executed. If there is more than one match, the first expression is
picked. If there is no match, again @racket[e-def] is run. 

Here is a sample use: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define (next-state-of-traffic-light current) 
  (dispatch current 
    [red        'yellow-red]
    [yellow-red 'green]
    [green      'yellow]
    [yellow     'red]
    [default 
     (if (symbol? current) 
         'blinking-red
          (error "really bad things happened"))]))
))
@;%
Of course, the expression in the clauses do not have to be symbols; they
could be proper expressions.}


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

