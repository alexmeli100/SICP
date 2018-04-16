#lang racket

(define (sum-of-two-squares pair) (+ (square (car pair)) (square (cadr pair))))

(define (stream-of-2-squares s weight) 
        (let ((wcar (weight (stream-car s)))
              (wcadr (weight (stream-cadr s)))
              (wcaddr (weight (stream-caddr s))))
            (if (and (= wcar wcadr) (= wcar wcaddr)) 
                (cons-stream (list (stream-car s)
                                   (stream-cadr s)
                                   (stream-caddr s)
                                   wcar)
                             (stream-of-2-squares (stream-cdddr s) weight))
                (stream-of-2-squares (stream-cdr s) weight))))