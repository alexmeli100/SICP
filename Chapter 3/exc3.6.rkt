#lang racket

(define (rand arg) 
    (define x random-int)
    (define (generate) (begin (set! x (rand-update)) x))
    (define (reset new-value) (set! x new-value))
    (cond ((eq? arg 'generate) generate)
          ((eq? reset 'reset)) reset))