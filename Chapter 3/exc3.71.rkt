#lang racket

(define (sum-pair-cubes pair) (+ (cube (car x)) (cube cadr x)))

(define ramujan-numbers (generate-ramujan-numbers sum-pair-cubes))

(define pairs (weighted-pairs integers integers sum-pair-cubes))

(define (generate-ramujan-numbers s weight) 
        (if (= (weight (stream-car s)) (weight (stream-cadr s))) 
            (cons-stream (list  (weight (stream-car s) (stream-car s) (stream-cadr s))) 
                         (generate-ramujan-numbers stream-cddr s weight))
            (generate-ramujan-numbers (stream-cdr s) weight)))