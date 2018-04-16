#lang racket

(define (ripple-carry a b s c) 
    (let ((Ci (make-wire))) 
        (if (null? a) 
            'ok
            (begin
                (full-adder (car a) (car b) c (car sum) Ci)
                (ripple-carry (cdr a) (cdr b) (cdr sum) Ci)))))