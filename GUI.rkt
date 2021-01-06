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
                   [width 365]
                   [height 325]))

(define table (new list-box%
                   [parent frame]
                   [choices '()]
                   [label "Results"]
                   [style (list 'single 'column-headers 'vertical-label)]
                   [columns (list "Trace name" "Nr Branches" "Correct Predictions" "Accuracy")]))

(define M (new slider%
               [parent frame]
               [label "Number of tables:"]
               [init-value 8]
               [min-value 4]
               [max-value 12]))

(define (get-result benchmarks nr-tables)
  (cons (list "FBUBBLE" "FMATRIX" "FPERM" "FPUZZLE" "FQUEENS" "FSORT" "FTREE" "FTOWER")
        (map (λ(x) (map number->string x)) (go-traces benchmarks nr-tables))))

(new button% [parent frame]
     [label "Go Traces"]
     [callback (λ(button event)
                 (send/apply table set
                             (get-result benchmarks (send M get-value))))])

(send frame show #t)