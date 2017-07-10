#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "mon-mor"]{From the Lambda Calculus to Redex}

@goals[

@item{What you will learn, how this might be useful}
@item{Basic Theory: reductions and calculations, @tt{eval}}
@item{from Theory to Redex}

]

@section{Workshop Goals}

Monday is about the very basics of operational models and semantics of
functional programming languages, that is, so-called syntactic theories. 

On Tuesday and Wednesday we will expand the coverage to imperative
languages, and you will learn to investigate the relationship between
syntax and semantics. Practitioners may take away ideas for approaching new
languages, instructors will see an entirely new way of teaching a course on
programming languages.

Thursday will be dedicated to building languages in Racket. 

Friday will tie together the Redex modeling approaches and the language
building approaches. 

This summer school cannot possible cover everything in the Racket school of
programming languages. For example, we will not show you how to work with
abstract machines. We will not show you how to create Racket languages that
use conventional syntax. And we will not work on performance issues of
models or language creation. By covering the wide spectrum of ideas,
however, we hope you get an idea of what Racket and its eco-system offers
and that you will take some time in the future to explore this world in
some depth. 

@; -----------------------------------------------------------------------------
@section{Theory: Trees, Substitution}

@bold{Terms vs Trees} The lambda calculus: most familiar, still misunderstood 

@ntt{e = x | (λx.e) | (e e)}

Terms vs trees, abstract over concrete syntax. Don't ever think of
@tt{(λx.(x (λy.y)))} as a string or sequence of chars; always think of it
as a tree: 
@verbatim[#:indent 4]{
    λx.
    | 
   app
   / \
  x   λy.
       |
       y
}

@bold{Substitution} From 8th grade math we know that functions are about
``plugging values in for variables.'' In our world, this is called
@defterm{substitution}. Notation: 

@ntt{ e[x <- e'] } pronounced: e with x replaced by e'

Substitution is tree surgery. But there are two ways to think about it: 

@ntt{ (λy.x)[x <- y] == λy.y } if taken literally. 

But @tt{λy.x} is like a constant function @tt{f(y) = x}. The result,
however, is a function that depends on its variable @tt{f(y) = y}. 

Here is the other way: 

@ntt{ (λy.x)[x <- y] == (λz.x)[x <- y] == λz.y }

Understand scope. Preserve bindings. And this is indeed the first task of
PL people, too. There are several ways to understand scope: 

@itemlist[

@item{define what it means for a variable to @defterm{bind} and in which
subtree the binding holds (free variable functions)}

@item{define a theory of renaming (α equivalence relation)}

@item{define equivalence classes of trees (based on α) and define
operations in terms of those.}
]

@; -----------------------------------------------------------------------------
@section{Redex: Languages, Scope, and Substitution}

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language Lambda1
  (e ::= x
         (lambda (x) e)
         (e e))
  (x ::= variable-not-otherwise-mentioned))
))
@;%

Here are the terms from above: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define e1 (term (lambda (x) y)))
(define e2 (term (lambda (z) y)))

(default-language Lambda1)

(term (substitute ,e1 y x)) 
(term (substitute ,e2 y x))
))
@;%
And these are the results we don't want. Because we did not specify scope
aka binding structure of our language. 

Let's fix that: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language Lambda
  (e ::= x
         (lambda (x) e)
         (e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language Lambda)
(term (substitute ,e1 y x))
(term (substitute ,e2 y x))
))
@;%
And now it all works out. 

Discuss @racket[alpha-equivalent?]

@; -----------------------------------------------------------------------------
@section{Theory: Laws of Calculation}

What are doing when we calculate? What do we mean when we write @math{=}, as in, 
@ntt{
  3 * (1 + 1)
= 3 * 2 
= 6 
}
We teach children that this is the ``truth'' but we can also just view
@math{=} or ``computes to'' as a relationship between terms. So @math{3 *
(1 + 1)} is a term, meaning a tree in our world: 
@verbatim[#:indent 4]{
    *
   / \ 
  3   +
     / \
    1   1
}
And the next term in the series is 
@verbatim[#:indent 4]{
   *
  / \ 
 3   2
}
Which just says that ``calculation is a relationship on terms.'' 

The lambda calculus is for calculating with functions instead of
numbers. So we need a relation that models the fundamental idea of function
application. Here are three of them: 
@itemlist[
 
@item{@ntt{ ((λx.e) e') beta-x e }

This one is totally weird. Functions always ignore their argument. That is
not what we want.}]

Here is what we want: 
@verbatim[#:indent 4]{
f(x) = 3 * (x + 1)
------------------
f(1) = 3 * (1 + 1)
     = 3 * 2 
     = 6 
}

@itemlist[
@item{@ntt{ ((λx.e) e') beta-y e[x <- e'] }

This one is better than the first but if we add arithmetic, odd things
happen: 
@verbatim[#:indent 4]{
g(x) = 42 
------------------
g(1/0) = 42  
}
Do we really want calculations to ignore errors? Do developers really want
their PL to throw away an erroneous computation? 

Not even the Lambda Calculus inventors were sure about this. They dabbled
in alternatives (for example the λI calculus) but could not resolve the
problem.}

@item{@ntt{ ((λx.e) (λx.e') ) beta-z e[x <- (λx.'e) ] }

This last relation resolves this issue by restricting the argument of a
function to be a function. In a world where everything is a function, this
seems okay.}
]
In the past 50 years, @tt{y} has become known as ``call by name'' (as in
Algol 60 or Simula 67) and @tt{z} has become known as ``call by value'' (as
in Pascal or Java). 

The basic relation is not enough to calculate. Every kid knows that you can
always ``substitute equals for equals'' in a term. So if we wish to
understand calculation from this fundamental perspective, we need to
generalize the basic relation to one that ``calculates wherever you want.''

Obviously we need to add reflexivity (@tt{e = e}), symmetry (@tt{e = e'
then e' = e}), and transitivity (@tt{e = e' and e' = e'' then e = e''}). 

But we also need to be able to do this anywhere in the tree. We use
@defterm{contexts} to express this idea. A context is result of removing a
sub-tree from a term term: 

@ntt{C = [] | (λx.C) | (C e) | (e C)}

@tt{[]} is called a @defterm{hole} and the only operation on contexts is to
fill the hole with a tree, i.e., to graft a tree onto the pruned branch: 

@ntt{C = (λx.[]) and e = (λy.x), then C[e] = (λx.(λy.x))}

Yes! Grafting a tree onto a context captures variables. That's
intentional. Watch: 

@ntt{e = (λz.(λy.z)) x.}

With beta-y, we get 

@ntt{e = (λy.x).}

But if this calculation takes place in some context 

@ntt{C = (λx.[])}

then we want to calculate like this: 

@ntt{C[(λz.(λy.z)) x] = C[(λy.x)]} 

regardless of what C is. 

In general, if @tt{e = e'}, then @tt{C[e] = C[e']}.

These are all the ingredients needed to understand the idea of
``calculating with functions.'' 

@; -----------------------------------------------------------------------------
@section{Redex: Contexts, Reductions, Graphs}

Let's extend our language with contexts: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-extended-language Lambda-calculus Lambda
  (C ::=
     hole
     (lambda (x) C)
     (C e)
     (e C)))
))
@;%

And let's express the simple-minded beta relation as a relation on this
calculus:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define ->beta
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((lambda (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        beta-name)))
))
@;%

There are two tools to explore this ``calculus'' now. The
@racket[apply-reduction-relation] function takes a step at a time: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(apply-reduction-relation ->beta (term (,e1 ,e2)))
))
@;%
Its cousin, @racket[apply-reduction-relation*], computes the transitive and
reflexive closure but not the symmetric one. 

Redex also comes with a tool that shows the reduction graph, that is, all
reductions possible starting from some term until no more reductions can be
applied: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define e3
  (term
   ((lambda (x) (lambda (y) x))
    ((lambda (x) x) z))))

(traces ->beta e3)
))
@;%

Here is the result: 
@centerline{@image{Images/traces1.png}}

@; -----------------------------------------------------------------------------
@section{Theory: From Calculation to Programming Languages}

(1) Programming language people are primarily interested in evaluating
programs, and secondarily in reasoning about evaluation. Calculations
supports both. It is good for the latter but is really miserable for the
former---because it requires search through a graph to find the
result. Fortunately, mathematicians prove (meta)theorems, and programming
language people know how to exploit those.

@bold{Theorem} If @tt{e} relates to @tt{e'} in a reduction graph, then
there exists a path of a particular shape. On this path, it is always the
leftmost-outermost reducible expression (@defterm{redex}) that is reduced. 

It turns out that this theorem holds for any sensible basic notion of
reduction: beta-y, beta-z and something called beta-need, which we did not
discuss. 

@bold{Corollary} An evaluator follows this path up to the first value
(function, closure). 

Here is now we say ``leftmost-outermost'' for beta-y and beta-z using
context notation: 

@ntt{E-y = [] | (E-y e)}

@ntt{E-z = [] | (E-z e) | ((λx.e) E-z)}


(2) Sensible programming language people do not want to encode numbers and
booleans and strings, they want to work with them directly. Let's add
numbers and the most primitive operation on them: if. Here we go: 

@ntt{e = x | (λx.e) | (e e) | tt | ff | (if e e e)}

This means we need two more relations and we need to union those with our
favorite beta relation: 

@ntt{(if tt e e') if-tt e} 
@ntt{(if ff e e') if-ff e'}

In the literature, you will find the name δ reduction for these things or
(external) interpretation function. 

Numbers are a bit more complicated: 

@ntt{e = x | (λx.e) | (e e) | <n> for all n in Integer | (e + e)}

The notation @tt{<n>} is called a @defterm{numeral}, a syntactic
representation of the number @tt{n}. When we write @tt{<n + 1>} we are
referring to the numeral that represents the successor of numeral
@tt{<n>}. So here is the δ reduction for numbers: 

@ntt{(<n> + <m>) arithmetic <n + m>}

@; -----------------------------------------------------------------------------
@section{Redex: Evaluators for Programming Languages}

Let's use evaluation contexts instead of plain contexts: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-extended-language Lambda-calculus PCF
  (E-name ::=
     hole
     (E-name e)))
))
@;%
And here is the evaluator: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction PCF-eval
  eval : e -> v
  [(eval e) ,(first (apply-reduction-relation* ->beta (term e)))])
))
@;%
An evaluator is a function from terms to values. This definition glosses
over some problems, which we will cover in the afternoon session. 

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language PCF
  (e ::=
     x
     (lambda (x) e)
     (e e)

     ;; booleans
     tt
     ff
     (if e e e)

     ;; arithmetic
     n
     (e + e))
  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))

(define-extended-language PCF-eval PCF
  (E-name ::=
     hole
     (E-name e)
     (E-name + e)
     (v + E-name))
  (v ::=
     n
     tt
     ff
     (lambda (x) e)))
))
@;%
Stop! Can you spot the bug in this definition? 

And here are the reductions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define ->name
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole E-name (if tt e_1 e_2))
        (in-hole E-name e_1)
        if-tt)
   (--> (in-hole E-name (if ff e_1 e_2))
        (in-hole E-name e_2)
        if-ff)
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name ,(+ (term n_1) (term n_2)))
        plus)))
))
@;%

Time to get your hands dirty. 

@section{Summary: Theory}

@nested[#:style 'inset
 @tabular[#:sep @hspace[5]
          #:row-properties '(bottom-border ())
  @list[
   @list[ @t{notation}   @t{meaning} ]
   @list[ @code{x}       @t{basic notion of reduction, without properties} ]
   @list[ @code{-->x}    @t{one-step reduction, generated from @code{x}, compatible with syntactic constructions} ]
   @list[ @code{-->>x}   @t{reduction, generated from @code{-->x}, transitive here also reflexive} ]
   @list[ @code{=x}      @t{``calculus'', generated from @code{-->x}, symmetric, transitive, reflexive} ]
   @list[ @tt{|-->x}   @t{standard reduction (one-step, multi-step), via meta-theorem} ]
   @list[ @tt{|-->>x}  'cont]
   @list[ @code{eval}    @t{functions, definable from @code{-->>x}, @code{=x}, or @tt{|-->>x}} ]
   @list[ @code{ }       @t{(equivalence from meta-theorems)} ]
   ]]]

@section{Summary: Redex}

@itemlist[
@item{@racket[define-language]}
@item{@racket[define-language] with @racket[#:binding-forms]}
@item{@racket[substitute]}
@item{contexts}
@item{@racket[reduction-relation]}
@item{@racket[eval]}
]
