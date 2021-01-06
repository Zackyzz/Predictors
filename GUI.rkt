#lang racket/gui

(require "O-GEHL.rkt")

(define benchmarks (list (file->lines "./traces/fbubble.tra")
                         (file->lines "./traces/fmatrix.tra")
                         (file->lines "./traces/fperm.tra")
                         (file->lines "./traces/fpuzzle.tra")
                         (file->lines "./traces/fqueens.tra")
                         (file->lines "./traces/fsort.tra")
                         (file->lines "./traces/ftree.tra")
                         (file->lines "./traces/ftower.tra")))

(define frame (new frame%
                   [label "O-GEHL"]
                   [x 500]
                   [y 100]
                   [width 300]
                   [height 300]))

(define table (new list-box%
                   [parent frame]
                   [choices '()]
                   [label "Results"]
                   [style (list 'single 'column-headers 'vertical-label)]
                   [columns (list "Trace name" "Accuracy")]))

(define results (go-traces benchmarks))

(define data (list
              (list "FBUBBLE" "FMATRIX" "FPERM" "FPUZZLE" "FQUEENS" "FSORT" "FTREE" "FTOWER")
              (map number->string results)))

(new button% [parent frame]
     [label "Go Traces"]
     [callback (λ(button event)
                 (send/apply table set data))])

(send frame show #t)