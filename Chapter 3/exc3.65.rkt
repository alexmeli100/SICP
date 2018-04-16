#lang racket

(define (log-summands n) 
    (cons-stream (/ 1.0 n)
                 (stream-map - (log-summands (+ n 1)))))

(define ln2-stream (partial-sums (log-summands 1)))

