#lang racket

(define (mult a b)
    (cond ((= b 0) 0)
          ((even? b) (mult (* a 2) (/ b 2)))
          (else (+ a (mult a (- b 1))))))

(mult 2 3)