#lang racket

(define benchmarks (list (file->lines "./traces/fbubble.tra")
                         (file->lines "./traces/fmatrix.tra")
                         (file->lines "./traces/fperm.tra")
                         (file->lines "./traces/fqueens.tra")
                         (file->lines "./traces/fpuzzle.tra")
                         (file->lines "./traces/fsort.tra")
                         (file->lines "./traces/ftower.tra")
                         (file->lines "./traces/ftree.tra")))

(define M 8)
(define C-WIDTH 4)

(define (count-digits n)
  (if (< n 2)
      1
      (+ 1 (count-digits (quotient n 2)))))

(define (get-index HRg PC Path index)
  (define L (if (= index 0) 0 (arithmetic-shift 1 (arithmetic-shift 1 index))))
  (if (= L 0)
      (bitwise-ior (arithmetic-shift PC (count-digits Path)) Path)
      (bitwise-ior (arithmetic-shift
                    (bitwise-ior
                     (arithmetic-shift (remainder HRg L) (count-digits PC))
                     PC)
                    (count-digits Path))
                   Path)))

#|(define (get-index-xor HRg PC T length)
  (define table-length (if (= length 0) 0 (arithmetic-shift 1 (arithmetic-shift 1 length))))
  (if (= table-length 0)
      PC
      (bitwise-xor (bitwise-xor (remainder HRg table-length) PC) T)))|#

(define (update-history HRg outcome)
  (remainder (bitwise-ior (arithmetic-shift HRg 1) outcome)
             (arithmetic-shift 1 (arithmetic-shift 1 (- M 1)))))

(define (saturated-counter value [range C-WIDTH])
  (define CW (arithmetic-shift 1 (- range 1)))
  (cond
    [(> value (- CW 1)) (- CW 1)]
    [(< value (- CW)) (- CW)]
    [else value]))

(define (modify-counter table index operation [new-val 1])
  (define value (operation (hash-ref table index 0) new-val))
  (hash-set! table index (saturated-counter value)))

(define (get-prediction HRg PC Path tables)
  (for/fold ([sum (/ M 2)])
            ([T tables] [i (in-range M)])
    (+ sum (hash-ref T (get-index HRg PC Path i) 0))))

(define (update-tables HRg PC Path correct? tables)
  (for ([T tables] [i (in-range M)])
    (modify-counter T (get-index HRg PC Path i) (if correct? + -))))

(define (go-trace trace)
  (define predictors (for/list ([i (in-range M)])
                       (make-hash)))
  (let loop ([HRg 0] [n-correct 0] [total 0] [clone trace])
    (cond
      [(empty? clone)
       (/ (ceiling (* (exact->inexact (/ (* 100 n-correct) total)) 100)) 100.0)]
      [else
       (define branch (string-split (first clone) " "))
       (define PC (string->number (second branch)))
       (define Path (string->number (third branch)))
       (define S (get-prediction HRg PC Path predictors))
       
       (define my-prediction (if (positive? S) 1 0))
       (define outcome (if (equal? (first branch) "NT") 0 1))
       (define comparison (= my-prediction outcome))

       (when (or (equal? #f comparison) (<= (abs S) M))
         (update-tables HRg PC Path comparison predictors))
       (loop (update-history HRg outcome)
             (if comparison (+ 1 n-correct) n-correct)
             (+ 1 total)
             (rest clone))])))

(define (go-traces benchmarks)
  (exact->inexact
   (/ (for/fold ([avg 0])
                ([trace benchmarks])
        (define accuracy (go-trace trace))
        (printf "~a\n" accuracy)
        (+ avg accuracy))
      (length benchmarks))))

(time (go-traces benchmarks))
