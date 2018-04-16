#lang racket

(define (print-queue queue) 
    (let ((ptr (front-ptr queue))) 
        (define (iter x) 
            (if (null? x) 
                (newline) 
                (begin 
                    (display (car x)) 
                    (iter (cdr x))))) 
        (iter ptr)))