#lang racket

(define  (pairs s t) 
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (interleave 
            (interleave 
                (stream-map (lambda (x) (list (stream-car s) x)) 
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t))) 
                            (stream-cdr s))) 
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2) 
    (if (null? s1) 
        s2
        (cons-stream (stream-car s1) 
                     (interleave s2 (stream-cdr s1)))))