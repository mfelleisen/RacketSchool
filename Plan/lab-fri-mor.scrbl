#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-fri-mor"]{@bold{Lab} Testing Models, Testing Languages}

@goals[
 @item{build a Records language} 
 @item{compare the Records specification}
]

@;common[]

@section[#:tag "lfm" #:style 'unnumbered]{Exercises}

@exercise["ex:model-vs-impl"]{Implement a @tt{#lang} that compiles
@tt{RacketSchool/Records1}. 

That is, develop a Racket compiler extension for the @tt{Records1}
language. The syntax is available from @secref{lab-tue-mor}; the semantics
of the various features is "obvious" and specified in Justin P.'s
model. The key is that record field retrieval must use a string literal.

Name the @tt{#lang} you develop @tt{RecImpl}. 

Use Matthew F.'s notes from @secref{thu-aft} as a guide to develop the
plug-in in an iterative fashion.}
