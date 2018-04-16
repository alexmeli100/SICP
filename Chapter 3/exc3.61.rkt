#lang racket

(define (invert-unit-series series) 
    (let ((negate-series (scale-stream (Stream-cdr series -1)))) 
        (define X (cons 1 (mul-series negate-series X)))
    X))