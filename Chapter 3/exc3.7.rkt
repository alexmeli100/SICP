#lang racket

#lang racket

(define (make-account balance password) 
    (define (withdraw amount pass) 
        (if (>= balance amount) 
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))

    (define (deposit amount pass) 
        (set! balance (+ balance amount)))
    (define (change-password new-pass) 
        (set! password new-pass))
    
    (define (dispatch m pass) 
        (cond (not (eq? password pass) (lambda (x) "Incorrect password"))
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'change-pass') change-password)
              (else (error "Unkown request -- MAKE ACCOUNT" m)))))

(define (make-joint account old-pass new-pass) 
    (begin 
        ((account 'change-pass) new-pass)
        account))