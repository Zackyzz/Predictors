#lang racket/gui

(require "O-GEHL.rkt" "L-TAGE.rkt")

(define benchmarks (list (file->lines "./traces/fbubble.tra")
                         (file->lines "./traces/fmatrix.tra")
                         (file->lines "./traces/fperm.tra")
                         (file->lines "./traces/fpuzzle.tra")
                         (file->lines "./traces/fqueens.tra")
                         (file->lines "./traces/fsort.tra")
                         (file->lines "./traces/ftree.tra")
                         (file->lines "./traces/ftower.tra")))

(define frame (new frame%
                   [label "State-of-the-art Predictors"]
                   [x 500]
                   [y 100]
                   [width 363]
                   [height 377]))

(define table (new list-box%
                   [parent frame]
                   [choices '()]
                   [label "Predictor Results:"]
                   [style (list 'single 'column-headers 'vertical-label)]
                   [columns (list "Trace name" "Nr Branches" "Correct Predictions" "Accuracy")]))

(define O-M (new slider%
                 [parent frame]
                 [label "Number of O-GEHL tables:"]
                 [init-value 8]
                 [min-value 4]
                 [max-value 12]))

(define L-M (new slider%
                 [parent frame]
                 [label "Number of L-TAGE tables:"]
                 [init-value 8]
                 [min-value 4]
                 [max-value 12]))

(define (get-result benchmarks nr-tables predictor)
  (cons (list "FBUBBLE" "FMATRIX" "FPERM" "FPUZZLE" "FQUEENS" "FSORT" "FTREE" "FTOWER")
        (map (λ(x) (map number->string x)) (predictor benchmarks nr-tables))))

(new button% [parent frame]
     [label "Predict with O-GEHL"]
     [callback (λ(button event)
                 (send table set-label "O-GEHL Results:")
                 (send/apply table set
                             (get-result benchmarks (send O-M get-value) go->O-GEHL)))])

(new button% [parent frame]
     [label "Predict with L-TAGE"]
     [callback (λ(button event)
                 (send table set-label "L-TAGE Results:")
                 (send/apply table set
                             (get-result benchmarks (send L-M get-value) go->L-TAGE)))])

(send frame show #t)