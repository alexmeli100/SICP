#lang racket

(define (smooth input-stream) 
    (stream-map (lambda(x y)(/ (+ x y) 2)) input-stream (stream-cdr input-stream))) 
   
  (define (zero-crossings input-stream) 
    (stream-map sign-change-detector input-stream (stream-cdr input-stream))) 
   
  (define (smoothed-zero-crossing sense-data) 
    (zero-crossings (smooth sense-data))) 