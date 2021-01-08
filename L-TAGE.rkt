#lang racket

(provide go->L-TAGE)

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

(define (get-prediction HRg PC tables M)
  (let loop ([i (- M 1)])
    (define tag-val (hash-ref (list-ref tables i) (get-index HRg PC i) 0))
    (cond
      [(= i 0) (list tag-val i)]
      [(not (= 0 tag-val)) (list tag-val i)]
      [else (loop (- i 1))])))

(define (saturated-counter value [range WIDTH])
  (define CW (<< 1 (- range 1)))
  (cond
    [(> value (- CW 1)) (- CW 1)]
    [(< value (- CW)) (- CW)]
    [else value]))

(define (modify-counter table index operation [new-val 1])
  (define value (operation (hash-ref table index 0) new-val))
  (hash-set! table index (saturated-counter value)))

(define (update-tables HRg PC taken? tables index [M 8])
  (modify-counter (list-ref tables index) (get-index HRg PC index) (if taken? + -))
  (when (< index (- M 1))
    (modify-counter (list-ref tables (+ 1 index)) (get-index HRg PC (+ 1 index)) (if taken? + -))))

(define (go-trace trace [M 8])
  (define predictors (for/list ([i (in-range M)])
                       (make-hash)))
  (let loop ([HRg 0] [n-correct 0] [total 0] [clone trace])
    (cond
      [(empty? clone) (list total n-correct (/ (* 100.0 n-correct) total))]
      [else
       (define branch (string-split (first clone) " "))
       (define PC (string->number (second branch)))
       (define mathcing-component (get-prediction HRg PC predictors M))
       (define matching-val (first mathcing-component))
       
       (define my-prediction (if (positive? matching-val) 1 0))
       (define outcome (if (equal? (first branch) "NT") 0 1))
       (define comparison (= my-prediction outcome))
       
       (when (or (not comparison) (<= (abs matching-val) (/ M 2)))
         (update-tables HRg PC (= outcome 1) predictors (second mathcing-component) M))
       
       (loop (update-history HRg outcome M)
             (if comparison (+ 1 n-correct) n-correct)
             (+ 1 total)
             (rest clone))])))

(define (go->L-TAGE benchmarks [M 8])
  (apply map list (for/list ([trace benchmarks])
               (go-trace trace M))))