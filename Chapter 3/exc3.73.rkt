#lang racket

(define (RC R C dt) 
    (lambda (v i) 
        (add-streams (scale-stream i R) 
                     (integral (scale-stream i (/ 1 c)) v dt))))