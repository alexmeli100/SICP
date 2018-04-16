#lang racket

(define (partial-sums stream) 
    (let ((partial (cons-stream 1 (add-streams partial (stream-cdr stream)))))
        partial))