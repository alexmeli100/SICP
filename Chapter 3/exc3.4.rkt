#lang racket

(define (make-account balance password) 
    (define count 0)
    (define (withdraw amount pass) 
        (if (>= balance amount) 
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount pass) 
        (set! balance (+ balance amount)))
    
    (define (dispatch m) 
        (cond ((not (eq? pass password)) (begin (set! count (+ count 1)) "Wrong password"))
              ((> count 7) call-the-cops)
              ((eq? m 'withdraw)  withdraw)
              ((eq? m 'deposit)  deposit)
              (else (error "Unkown request -- MAKE ACCOUNT" m)))))