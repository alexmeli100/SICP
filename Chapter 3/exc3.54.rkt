#lang racket

(define (mult-stream stream1 stream2) 
    (stream-map * stream1 stream2))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))