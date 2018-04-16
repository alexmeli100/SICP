#lang racket

(define (exp b n)
    (cond ((= n 0) 1) 
          ((even? n) (square (exp b (/ n 2))))
          (else (* b (exp b (- n 1))))))
(define (square n) (* n n))

(exp 2 4)