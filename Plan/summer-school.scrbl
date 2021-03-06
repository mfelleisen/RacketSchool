#lang scribble/manual

@(require scriblib/figure)

@(require (for-label redex))

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "redex2015"]{The Racket School of Semantics and Languages 2017}

@margin-note{
Matthias Felleisen, 
Robert Bruce Findler, 
Matthew Flatt, 
Shriram Krishnamurthi, 
Jay McCarthy, 
and Justin Pombrio}

This summer school introduces attendees to the Racket philosophy of
 analyzing, modeling, and building languages. 

The table below presents a concise summer school schedule. @emph{Most} sessions
 will consist of a lecture part and a hands-on lab. Our goal is to lecture
 for less than 90 minutes and to give you at least 120 minutes of lab
 time. The lectures will introduce basic ideas; the labs will present
 practical exercises on these topics. We will have a break between lectures
 and labs.

By Wednesday afternoon, you might be exhausted from the breakneck speed of
 the school. Shriram K. will lecture on his experience with semantics
 re-engineering, but no lab is scheduled. Instead, feel free to hike a
 mountain, swim in the salt lake, or explore the U and SLC in any way you
 feel comfortable. If you'd rather hang around and work on something, some
 of us will be available to help you along.

Thursday afternoon Matthew F. will take you from extending languages to
 building complete languages. You will use a small amount of this material
 on Friday morning to build your first "hash lang."

Robby F.'s Friday afternoon lecture will cover his experience with random
 testing and how to best use Redex's current random-testing facilities.
 Again, no lab is scheduled to follow this lecture. Instead, feel free to
 pick our individual and collective brains about various issues concerning
 languages, semantics, teaching and other related issues.

@bold{Postscript} These notes have been adjusted to reflect the actual
 summer school schedule and have been updated with links to code and other
 materials used during the lectures and labs. 


@; -----------------------------------------------------------------------------
@(define (blank) @list[@t{ } 'cont 'cont])
@figure["fig:schedule" @list{Workshop schedule}]{
@tabular[
 #:sep @hspace[3]
 #:row-properties '(bottom-border top)
 #:column-properties '(left)
@list[
@list[@t{ }          @t{Morning (9:00 -- 12:30)}          @t{Afternoon (13:30 -- 17:00)}]
@; -------------------------------------------------------------------------------------
@list[ @t{Monday}    @t{@secref{mon-mor}}                @secref{mon-aft}]
@list[ @t{ }         @t{@secref{lab-mon-mor}}            @secref{lab-mon-aft}]

@blank[]
@; -------------------------------------------------------------------------------------
@list[ @t{Tuesday}   @secref{tue-mor}                    @secref{lab-tue-aft}]
@list[ @t{ }         @secref{lab-tue-mor}	         @t{ }]

@blank[]
@; -------------------------------------------------------------------------------------

@list[ @t{Wednesday} @secref{wed-mor}                    @secref{lab-wed-mor}]
@list[ @t{ }         @secref{lab-wed-event}	         @secref{wed-aft}]

@blank[]
@; -------------------------------------------------------------------------------------
@list[ @t{Thursday}  @secref{thu-mor}                    @secref{thu-aft}]
@list[ @t{ }         @secref{lab-thu-mor}	         @secref{lab-thu-aft}]

@blank[]
@; -------------------------------------------------------------------------------------
@list[ @t{Friday}    @secref{lab-fri-mor}		@secref{fri-aft}]
@list[ @t{ }         @secref{fri-mor} 			@t{@italic{consult with staff}}]
]]}

@include-section{pre-mon-mor.scrbl} @include-section{lab-mon-mor.scrbl}
@include-section{pre-mon-aft.scrbl} @include-section{lab-mon-aft.scrbl}

@include-section{pre-tue-mor.scrbl} @include-section{lab-tue-mor.scrbl}
@;include-section{pre-tue-aft.scrbl}

@include-section{lab-tue-aft.scrbl}

@include-section{pre-wed-mor.scrbl}
@include-section{lab-wed-event.scrbl}
@include-section{lab-wed-mor.scrbl}
@include-section{pre-wed-aft.scrbl} @; --- 

@include-section{pre-thu-mor.scrbl} @include-section{lab-thu-mor.scrbl}
@include-section{pre-thu-aft.scrbl} @include-section{lab-thu-aft.scrbl}

@include-section{lab-fri-mor.scrbl} @include-section{pre-fri-mor.scrbl}
@include-section{pre-fri-aft.scrbl} @; --- 
