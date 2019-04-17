#lang scribble/base
@(require scriblib/figure
          pict
          "survey-responses.rkt")

@title{Report on the 2018 Racket Summer School}


@figure["fig:histograms" "Histograms of Numeric Question's Answers"]{
 @(vc-append (ht-append 30
                        (histogram 1)
                        (histogram 2))
             (histogram 3))
}


@section{Responses to the question “@(free-text-question 4)”}

@(free-text-responses 4)

@section{Responses to the question “@(free-text-question 5)”}

@(free-text-responses 5)
