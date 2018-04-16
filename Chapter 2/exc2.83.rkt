#lang racket

(define (install-scheme-number-package) 
    ;; ...
    
    (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
    ;; ...
    )

(define (install-rational-package) 
    ;; ...
    (define (raise x) (/ (numer x) (denom x)))
    
    ;; ...

    (put 'raise '(rational) raise)
    )

(define (raise x) (apply-generic 'raise x))