#lang scribble/manual

@(require "shared.rkt")
@(require "fri-diagram.rkt")

@; -----------------------------------------------------------------------------
@title[#:tag "fri-mor"]{Specification vs Implementation} 

@goals[
 @item{building a language from specification}
 @item{testing languages against specifications}
]

@section{Setting up a test bed}

The goal of this morning's session is to develop a Racket compiler
extension for the @tt{Records1} language. The syntax is available from
@secref{lab-tue-mor}; the semantics of the various features is "obvious"
and specified in Justin P.'s model. The key is that record field retrieval
must use a string literal. 

Name the @tt{#lang} you develop @tt{RecImpl}. 

Use Matthew F.'s notes from @secref{thu-aft} as a guide to develop the
plug-in in an iterative fashion. 

When you have gotten far enough, I will release a revised version of this
web page so we can discuss and implement @figure-ref{model-vs-impl}. 

@figure["model-vs-impl" @list{Comparing models and implementations}]{
 @centerline{@impl-vs-spec[.6]}
}
