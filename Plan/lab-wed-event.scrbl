#lang scribble/manual

@(require "shared.rkt")

@(define basics 
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/public/basic.rkt")

@(define basics+
  "https://raw.githubusercontent.com/mfelleisen/RacketSchool/master/Exercises/mystery-semantics-variables.rkt")

@(define event-loop
  "https://raw.githubusercontent.com/justinpombrio/RacketSchool/master/private/event-loop.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-wed-event"]{@bold{Lab} Modeling Event Loops}

@goals[
@item{developing a model of event loops, like those for JS}
]

Justin P. and Shriram K. developed and presented a model of JavaScript's
event loop.

Our model deals with two kinds of events: 
@itemlist[
@item{@tt{key} events happen when the user presses a key, and}
@item{a @tt{resume} event returns execution to running a thread.} 
]
These are assumed to happen in chunks, with time in between each
chunk; for instance, the user may type `h' `i', wait a while, and then
type `w' `o' `r' `l' `d'. We choose to represent such sequences as lists of
lists. In particular, the example is represented as
@centerline{@tt{(((key "h") (key "i")) ((key "w") (key "o") (key "r") (key "l") (key "d")))}.} 
(This is a modelling choice; there are many other sensible choices.)

Since it is important that @tt{key} events get processed eventually, a
@tt{resume} event forces a process to @italic{yield} and allow other events
to be processed. In the Redex model, this is represented by pushing a
@tt{resume} event onto the end of the current chunk.

@section[#:tag "lwe" #:style 'unnumbered]{Exercises}

@exercise["ex:try-event-loop"]{Download the @hyperlink[event-loop]{event
  loop Redex model}. Try running the examples: @tt{(traces event-loop->
  t-event{1,2,3,4})}, or write your own example and view its trace. Read
  the e-event and e-yield reductions in the Redex model, and see if you can
  make sense of them.  }

@exercise["ex:fix-event-loop"]{This event loop allows @italic{starvation};
 @margin-note*{This exercise is due to Sorawee Porncharoenwase.}
  that is, if the @tt{resume} thread runs forever, then the next chunk of
  @tt{key} events will never be processed, even if @tt{resume} yields
  infinitely often.  Can you fix this?

@bold{Note} This question is open-ended. There is an easy fix, and there
are fixes that suggest interesting implementations.}
