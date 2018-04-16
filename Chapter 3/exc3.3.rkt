#lang racket

(define (make-account balance password) 
    (define (withdraw amount pass) 
        (if (>= balance amount) 
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount pass) 
        (set! balance (+ balance amount)))
    
    (define (dispatch m pass) 
        (cond (not (eq? password pass) (lambda (x) "Incorrect password"))
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unkown request -- MAKE ACCOUNT" m)))))