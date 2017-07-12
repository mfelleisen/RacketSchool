#lang scribble/manual

@(require "shared.rkt")
@;(define redex-eval (let ([e (make-base-eval)]) (e '(require redex)) e))

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "mon-aft"]{Modeling Functional Expression Languages}

@goals[

@item{matching terms}
@item{meta-functions}
@item{TDD for meta-functions and relations}

]

@; -----------------------------------------------------------------------------
@section{Dealing with Terms and Syntax}

Let's step back and look at language modeling slowly. 

After you have a syntax, use the grammar to generate instances and check
them (typos do sneak in). Instances are generated with @racket[term]: 
@;
@;interaction[#:eval redex-eval
@racketblock[
(define e1 (term y))
(define e2 (term (lambda (y) y)))
(define e3 (term (lambda (x) y)))
(define e4 (term (,e2 ,e3)))
]
Mouse over @racket[define]. It is @emph{not} a Redex form, it comes from
Racket. Take a close look at the last definition. Comma anyone? Enter
@racket[e4] at the REPL and see what it says. 

How about checking whether a term belongs to a syntactic category in a
language? 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(redex-match? Lambda e e4)
))
@;%

Define yourself a predicate that tests membership: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define Lambda? (redex-match? Lambda e))
))
@;%
Now you can formulate language tests: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(test-equal (Lambda? e1) #true)
(test-equal (Lambda? e2) #true)
(test-equal (Lambda? e3) #true)
(test-equal (Lambda? e4) #true)

(define eb1 (term (lambda (x x) y)))
(define eb2 (term (lambda (x) 3)))

(test-equal (Lambda? eb1) #false)
(test-equal (Lambda? eb2) #false)

(test-results)
))
@;%
Make sure your language contains the terms that you want and does
@emph{not} contain those you want to exclude. Why should @racket[eb1] and
@racket[eb2] not be in @racket[Lambda]'s set of expressions? 

@; NO because (lambda (x x) is not in grammar
@; NO because 3 is not in grammar

@; -----------------------------------------------------------------------------
@section{Meta-functions}

@bold{Rule} Avoid escaping to Racket. Racket is a general-purpose language,
and Redex is a language about terms.

But, language models need meta-functions in addition to reduction
relations. We have seen one such function in action:
@racket[substitute]. It is a meta-function, i.e., a function that maps
terms to terms. It is a bit unusual, because it is also needs to be told
(via @racket[default-language]) which language to deal with.

@;{The current version of Monday morning's plan also uses `eval`.}

Whenever Redex encounters the use of a meta-function in a @racket[term]
context, it evaluates it automatically. It does not need any extra hints. 

Language modelers want to define their own meta-functions. Here is the
name-giving example: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(--> (in-hole E-name (n_1 + n_2))
     (in-hole E-name (plus n_1 n_2))
     plus)
))
@;%
Remember that the right-hand side of a relation is a term position. When
Redex finds the application of a meta-function there, it evaluates it. So
now we need to define the meta-function @racket[plus] to get PCF back: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction PCF-eval
  plus : n n -> n
  [(plus 1 1) 2]
  [(plus n n) 3]
  [(plus n_1 n_2) 1])
))
@;%
A meta-function is specified via pattern matching. The right-hand side
of each case is a @racket[term] context. Just like for reduction relations,
we could escape to Racket via a comma (unquote). 

This might not be what you had in mind, but nobody said PCF's number system
is the one from Racket. @margin-note*{As a matter of fact, it is @emph{not}.}

A second example is a function that helps Falsify your language. Let's say
we want @tt{ff} and @tt{0} to stand for @racket[false]. We could add
another reduction rule or we could use a meta-function to express the idea
that @racket[if] coerces the test value:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
   (--> (in-hole E-name (if v e_1 e_2))
        (in-hole E-name e_1)
        (where tt (falsify v))
        if-tt)
   (--> (in-hole E-name (if v e_1 e_2))
        (in-hole E-name e_2)
        (where ff (falsify v))
        if-ff)
))
@;%
The @racket[where] clause imposes a side condition on the relation. Its
right-hand side is a term position; if Redex finds the application of a
meta-function there, it evaluates it. 

Here is the obvious definition of @racket[falsify]: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction PCF-eval
  falsify : v -> tt or ff
  [(falsify 0)  ff]
  [(falsify ff) ff]
  [(falsify v)  tt])
))
@;%

A final example is @racket[eval]:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction Lambda-calculus 
  eval : e -> v
  [(eval e) ,(first (apply-reduction-relation* ->name (term e)))])
))
@;%
Ouch, we just escaped to Racket. Let's reduce the use of Racket: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction Lambda-calculus 
  eval-1 : e -> v
  [(eval-1 e) v
   (where (v) ,(apply-reduction-relation* ->name (term e)))])
))

But look what happens if we use @racket[eval] on @tt{(y (lambda (x) x))}. It
blows up; the codomain test fails. Going back to Plotkin's seminal paper
(1974), nobody uses terms with free variables as programs. So, we want a
definition of @racket[eval] that fixes this. This needs two things: (1)
side-conditions on clauses and (2) a function that retrieves the free
variables from a term.

Here is how we do the first step: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction Lambda-calculus
  eval-2 : e -> v or (unbound-variables (x ...))
  [(eval-2 e)
   v_one-term 
   (where () (fv e))
   (where (v_one-term) ,(apply-reduction-relation* ->name (term e)))]
  [(eval-2 e)
   (unbound-variables (fv e))])
))
@;%
The function has two clauses now. The first one is protected with two
@racket[where] constraints: the first one ensures @racket[e] is closed and
the second one checks that reducing @racket[e] produces only one
result. The second clause of @racket[eval-2] ignores the input and signals
a ``compile-time'' error.@margin-note*{Compilers often call this
``undefined variable.''} 

From the context, we can guess that @racket[fv] retrieves the list of free
variables from a term @racket[e]. So this is its header: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; retrieve the list of free variables from e
(define-metafunction Lambda-calculus
  fv : e -> (x ...))
))
@;%
The function traverses @racket[e] in a syntax-directed manner of course:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; retrieve the list of free variables from e
(define-metafunction Lambda-calculus
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (lambda (x) e)) (minus (fv e) x)]
  [(fv (e_1 e_2)) (union (fv e_1) (fv e_2))])
))
@;%
If you have ever seen such a function before, you know that this is very
much standard---@racket[minus] subtracts a variable from a list and
@racket[union] combines two such lists. 

For @racket[union], we cheat: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction Lambda-calculus
  union : (x ...) (x ...) -> (x ...)
  [(union (x_1 ...) (x_2 ...)) (x_1 ... x_2 ...)])
))
@;%

For @racket[minus], we show off a cool feature of Redex pattern matching: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction Lambda-calculus
  minus : (x ...) x -> (x ...)
  [(minus (x_1 ... x x_2 ...) x) (minus (x_1 ... x_2 ...) x)]
  [(minus (x_1 ...) x) (x_1 ...)])
))
@;%

@; -----------------------------------------------------------------------------
@section{Testing}

But we really want test-driven development of these
functions. Here is how:  
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(test-equal (term (minus () x)) (term ()))
(test-equal (term (minus (x) x)) (term ()))
(test-equal (term (minus (x y) x)) (term (y)))
(test-equal (term (minus (z w x y) x)) (term (z w y)))
))
@;%
If you want to develop tests before you code, or if you want to illustrate
functions with tests before people read the code, use @racket[(module+ test ...)].

And we want tests for our reduction relations: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(test--> ->name e3 e3-evaluated)
(test--> ->name e3 (term z))
))
@;%
The first succeeds using @racket[alpha-equivalent?] implicitly; the second
fails (as expected). The @racket[test-->>] form tests the transitive closure
of the relation. 
