#lang racket

(define (stream-limit stream tolerance) 
    (if (< (abs (- (stream-car stream) (stream-cadr stream)))) 
        (stream-cadr stream)
        (stream-limit (stream-cdr stream) tolerance)))