#lang racket

(define (f n)
    (if ( < n 3)
    n
    (+ (f (- n 1)) (* (f (- 2 n)) 2) (* (f (- 3 n)) 3))))



(f 10)
