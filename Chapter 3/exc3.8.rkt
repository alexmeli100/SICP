#lang racket

(define f 
    (let ((called #f)) 
      (lambda (x) 
        (if called 
            0 
            (begin 
              (set! called #t) 
              x))))) 