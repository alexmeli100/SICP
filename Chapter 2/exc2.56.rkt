#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) 
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend x) (cadr x))
(define (augend x) (caddr x))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list ('+ a1 a2)))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

(define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list ('* a1 a2)))))



(define (exponent? x) (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation base exp)
    (cond ((=number? base 1) 1)
          ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list ('** base exp)))))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ((sum? exp)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
          ((product? exp)
            (make-sum 
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
          ((exponent? exp)
            (make-product 
                (make-product (exponent exp)
                              (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                (deriv (base exp) var)))

          (else (error "unkown expression type -- DERIV" exp))))

(deriv (list ('** 'x 3)) 'x)