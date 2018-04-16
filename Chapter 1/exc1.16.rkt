#lang racket

(define (expt b n)
    (expt-iter 1 b n))

(define (expt-iter product b counter)
    (cond ((= counter 0) product)
          ((even? counter) (expt-iter product (square b) (/ counter 2) ))
          (else  (expt-iter (* product b) b (- counter 1) ))))

(define (square n) (* n n))

(expt 4 4)
