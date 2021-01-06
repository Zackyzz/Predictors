#lang racket

(provide go-traces)

(define WIDTH 4)
(define MAX 32)

(define (<< n m) (arithmetic-shift n m))
(define (|| n m) (bitwise-ior n m))
(define (^ n m) (bitwise-xor n m))

(define (update-history HRg outcome [M 8])
  (remainder (|| (<< HRg 1) outcome)
             (<< 1 (<< 1 (- M 1)))))

(define (get-index HRg PC index)
  (define L (if (= index 0) 0 (<< 1 (<< 1 index))))
  (if (= L 0)
      PC
      (^ (remainder HRg L) PC)))

(define (get-prediction HRg PC tables [M 8])
  (for/fold ([sum (/ M 2)])
            ([T tables] [i (in-range M)])
    (+ sum (hash-ref T (get-index HRg PC i) 0))))

(define (saturated-counter value [range WIDTH])
  (define CW (<< 1 (- range 1)))
  (cond
    [(> value (- CW 1)) (- CW 1)]
    [(< value (- CW)) (- CW)]
    [else value]))

(define (modify-counter table index operation [new-val 1])
  (define value (operation (hash-ref table index 0) new-val))
  (hash-set! table index (saturated-counter value)))

(define (update-tables HRg PC taken? tables [M 8])
  (for ([T tables] [i (in-range M)])
    (modify-counter T (get-index HRg PC i) (if taken? + -))))

(define (go-trace trace [M 8])
  (define predictors (for/list ([i (in-range M)])
                       (make-hash)))
  (define TC 0)
  (define theta M)
  (let loop ([HRg 0] [n-correct 0] [total 0] [clone trace])
    (cond
      [(empty? clone) (list total n-correct (/ (* 100.0 n-correct) total))]
      [else
       (define branch (string-split (first clone) " "))
       (define PC (string->number (second branch)))
       (define S (get-prediction HRg PC predictors M))
       
       (define my-prediction (if (positive? S) 1 0))
       (define outcome (if (equal? (first branch) "NT") 0 1))
       (define comparison (= my-prediction outcome))

       (when (not comparison)
         (set! TC (+ TC 1))
         (when (> TC MAX)
           (set! TC MAX)
           (when (< theta (* 2 MAX))
             (set! TC 0)
             (set! theta (+ theta 1)))))
       (when (and comparison (<= (abs S) theta))
         (set! TC (- TC 1))
         (when (< TC (- MAX))
           (set! TC (- MAX))
           (when (> theta 0)
             (set! TC 0)
             (set! theta (- theta 1)))))

       (when (or (not comparison) (<= (abs S) theta))
         (update-tables HRg PC (= outcome 1) predictors M))
       
       (loop (update-history HRg outcome M)
             (if comparison (+ 1 n-correct) n-correct)
             (+ 1 total)
             (rest clone))])))

(define (transpose matrix)
  (for/list ([i (length (list-ref matrix 0))])
    (for/list ([j matrix])
      (list-ref j i))))

(define (go-traces benchmarks [M 8])
  (transpose (for/list ([trace benchmarks])
               (go-trace trace M))))