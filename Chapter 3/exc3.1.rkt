#lang racket

(define (make-accumulator x) 
    (define (accumulator y) 
        (begin (set! x (+ x y))
               x))
    accumulator)

(define A (make-accumulator 5))

(A 10)