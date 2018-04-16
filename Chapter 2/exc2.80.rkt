#lang racket

(define (=zero? x y) (apply-generic '=zero x y))

(define (install-scheme-number-package) 
    ;; ...
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))

(define (install-rational-package) 
    ;; ...

    (define (=zero? x) (= (numer x) 0))

    (put 'zero? '(rational) =zero?) 'done)

(define (install-complex-package) 
    ;; ...
    (define (=zero? x) 
        (and (= (real-part x) 0) (= (imag-part) 0)))
    (put '=zero? '(complex) =zero?)
    'done)