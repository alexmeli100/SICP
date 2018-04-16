#lang racket

(define (make-monitored f) 
    (define counter 0)        
    (define (dispatch args) 
        (cond ((eq? args 'how-many-calls) counter) 
              ((eq? args 'reset-count) (set! counter 0)) 
              (else (set! counter (+ counter 1)) 
                    (f args))))
    dispatch)

(define s (make-monitored sqrt))

