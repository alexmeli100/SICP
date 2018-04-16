#lang racket

(define (install-complex-package) 
    ;; ...
    (define (project x) (make-real (imag-part x)))

    (put 'project '(complex) project)
    )

(define (install-real-package) 
    ;; ...
    (define (project x) (make-scheme-number (round x) ))

    (put 'project '(real) project)
    )
    

(define (install-rational-package) 
    ;; ...
    (define (project x) (make-scheme-number (round (/ (numer x) (denom x)))))

    ;; ...

    (put 'project '(rational) project)
    )


(define (install-scheme-number-package) 
    ;; ...
    (define (project x) x)

    (put 'project '(scheme-number) project)

    )



(define (drop x) 
    (if (eq? (raise (project x)))  
        (drop (project x))
        x))

(define (project x) (apply-generic 'project x))
