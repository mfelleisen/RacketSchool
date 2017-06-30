#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "mon-mor"]{Semantics and Redex}

@goals[

@item{Basic Theory: reductions and calculations, @tt{eval}}
@item{from Theory to Redex}

]

@; -----------------------------------------------------------------------------
@section{Theory: Trees, Substitution}

@bold{Terms vs Trees} The lambda calculus: most familiar, still misunderstood 

@ntt{e = x | (\x.e) | (e e)}

Terms vs trees, abstract over concrete syntax

Encode some forms of primitives: numbers, booleans---good for theory of
computation; mostly irrelevant for PL. But let's continue. 

@bold{Substitution} From 8th grade math we know that functions are about
``plugging values in for variables.'' In our world, this is called
@defterm{substitution}. Notation: 

@ntt{ e[x <- e'] } pronounced: e with x replaced by e'

Substitution is tree surgery. But there are two ways to think about it: 

@ntt{ (\y.x)[x <- y] == \y.y } if taken literally. 

But @tt{\y.x} is like a constant function @tt{f(y) = x}. The result,
however, is a function that depends on its variable @tt{f(y) = y}. 

Here is the other way: 

@ntt{ (\y.x)[x <- y] == (\z.x)[x <- y] == \z.y }

Understand scope. Preserve bindings. And this is indeed the first task of
PL people, too. There are several ways to understand scope: 

@itemlist[

@item{define what it means for a variable to @defterm{bind} and in which
subtree the binding holds (free variable functions)}

@item{define a theory of renaming (α equivalence relation)}

@item{define equivalence classes of trees (based on α) and define
operations in terms of those.}
]

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

Discuss @racket[alpha-equivalence?]

@; -----------------------------------------------------------------------------
@section{Theory: Laws of Computation}

@ntt{ ((\x.e) e') beta-name e[x <- e'] }

substitute equals for equals. Calculate wherever you want. Contexts. 

@ntt{ ((\x.e) v) beta-value e[x <- v ] }

@; -----------------------------------------------------------------------------
@section{Redex: Contexts, Reductions, Graphs}

@; -----------------------------------------------------------------------------
@section{Theory: From Computation to Programming Languages}

Extensions with primitive data

@ntt{e = x | (\x.e) | (e e) | tt | ff | (if e e e)}

Here are two more, often done via external interpretation functions (δ)

@ntt{(if tt e e') if-tt e} 
@ntt{(if ff e e') if-ff e'}

If this is supposed to be a theory of functions (and if expressions) we
need to be able to use this relations @emph{in context} 

@verbatim[#:indent 4]{
      e xyz e'    
  --------------
      e  =  e'  

      e = e'                 e = e'                 e = e'    
  --------------         --------------	        --------------
  e e'' = e' e''         e'' e = e'' e'	         \x.e  = \x.e'

  plus reflexivity, symmetry, and transitivity 
}
for any relation @tt{xyz}

Now you have an equational system. what's it good for? you can prove such
facts as 

@ntt{e (Y e) = (Y e)}

meaning @emph{every single term has a fixpoint}

All of the above is mathematics but it is just that, mathematics. It might
be considered theory of computation, but it is @emph{not} theory of
programming languages. But we can use these ideas to create a theory of
programming languages. 

@; -----------------------------------------------------------------------------
@section{1958--1972}

Many people brought the lambda calculus into PL. They had good intuition,
but they did not know what to make of it all. Most used the syntax, some
appealed to the equations. The latter did not know what they were doing.

@margin-note{Mathematicians, too, had an idea that the lambda calculus was
 a bit misconceived. They tried requiring that every lambda-bound variable
 shows up at least once in the function's body. This doesn't address the
 problem.} 

One of the sticky points was Algol 60's introduction in 1958 of
call-by-value versus call-by-name, and the acknowledgment of others that
there was a real distinction. But they couldn't figure it out. 

Instead they proposed that call-by-value was a mere strategy of
implementing the lambda calculus, often dubbed applicative-order evaluation
(strategy). People considered the above the @emph{true} idea of lambda
calculus. @bold{Anybody who uses this word does not know what he's talking
about.}

Sadly, just when the issue was about to be resolved once and for all,
several different practitioners took off and engraved this ill-defined
terminology on the broad communities mind. 

A decade=plus later, others invented the almost equally silly terminology
of weak head reduction, resurrecting the head-reduction idea from
mathematics and somehow trying to construct a PL theory from that. Again,
people who use this terminology---and the closely related
small-step/big-step stuff---do not understand the rest of this framework
and never bothered to read the literature properly. 

@; -----------------------------------------------------------------------------
@section{1974}

Plotkin's 1974 TCS paper on call-by-name versus call-by-value shows how to
resolve this problem once and for all. 

In addition, Plotkin's paper also sketches several research programs,
mostly on scaling up his ideas to the full spectrum of languages but also
on the precise connection between by-value and by-name their relationship,
both at the proof-theoretical level as well as at the model-theoretic
level.

Here is Plotkin's idea as a quasi-algorithm: 
@itemlist[#:style 'ordered

@item{Start from an abstract syntax, plus notions of scope and
scope-preserving substitution. Consider closed terms @deftech{Program}s.}

@item{Identify a subset of expressions as @deftech{Value}s. Use @tt{v} to
range over @tech{Value}s.

@bold{Note} The complement of this set was (later) dubbed
@deftech{computations}, due to Moggi's work under Plotkin.}

@item{Define basic notions of reduction (axioms). Examples: 

@ntt{((\x.e) e') beta-name e[x=e']}
@ntt{((\x.e) v) beta-value e[x=v]}

}

@item{Inductively generate an equational theory from the basic notions of
reduction.}

@item{This theory defines a semantics, that is, a relation @italic{eval}
from programs to values:

@ntt{eval : Program x Value}
@ntt{@bold{def} e eval v iff e = v}
}

@item{Prove that @italic{eval} is a function, and you have got yourself a
@emph{specification} of an interpreter.

@ntt{eval : Program -> Value}
@ntt{eval(e) = v}

@bold{Note} This step often reuses a variant of the Church-Rosser theorem
of the mathematical theory of lambda calculus.}

@item{Prove that the calculus satisfies a Curry-Feys standard reduction
property. This gives you a second semantics: 

@ntt{eval-standard : Program -> Value}
@ntt{@bold{def} eval-standard(e) = v iff e standard reduces to v}

The new semantics is correct:
@ntt{@bold{Theorem} eval-standard = eval}

@deftech{Standard reduction} is a strategy for the lambda calculus, that
is, a function that picks the next reducible expression (called
@deftech{redex}) to reduce. Plotkin specifically uses the
leftmost-outermost strategy but others may work, too.}

]
Plotkin also shows---on an ad hoc basis---that this evaluator function is
equivalent to Landin's evaluator based on the SECD machine, an abstract
register machine.

Plotkin (following Morris, 1968) uses step 6 from above to add two ideas: 
@itemlist[

@item{The interpreter of a programming language (non-constructively)
 generates a theory of equivalence on phrases. 

@ntt{@bold{def} e ~ e' iff placing e and e' into any context yields
programs that produce the same observable behavior according to eval}

@bold{Theorem} @tt{~} is the coarsest equivalence theory and thus unique.

Let's call @tt{~} the @deftech{Truth}.}

@item{@bold{Theorem} e = e' implies e ~ e'. Naturally the reverse doesn't hold.}
]

@; -----------------------------------------------------------------------------
@section{1986--1992}

Matthias's (post)dissertation research extends Plotkin's work in two
directions: 
@;
@itemlist[#:style 'ordered

@item{Plotkin's ``algorithm'' applies to imperative programming language,
 especially those extending the lambda calculus syntax with (variable)
 assignment and non-local control operators.

@Secref{wed-mor} explains how two of these work.}

@item{It is possible to derive useful abstract register machines from the
standard reduction semantics of the programming language

Each machine @tt{M} defines a new semantics: 
@ntt{@bold{def} eval-M(e) = v iff load M with e, run, unload, yields v}

For each of these functions, we can prove an equivalence theorem.

@ntt{@bold{Theorem} eval-M = eval-standard = eval}}

]
@;
His work also shows how this approach greatly simplifies proofs of
consistency for the semantics of programming languages and especially
so-called type soundness theorems. 

@; -----------------------------------------------------------------------------
@section{1999--2010}

Matthew maintains and extends the monograph that collects the above ideas. 

Robby creates, implements, and maintains Redex with his PhD students. 

While Redex was developed to facilitate the development of PL models
according to the above framework, it has become useful above and beyond
this application domain. David Van Horn will demonstrate one such
application, just one step beyond the framework. Others have gone much
further still. 

Feel encouraged to stretch the limits of Redex. 
