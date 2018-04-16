#lang racket

  
(define (negate x) (apply-generic 'negate x)) 
  
;; add into scheme-number package 
(put 'negate 'scheme-number 
      (lambda (n) (tag (- n)))) 
 
;; add into rational package 
(put 'negate 'rational 
     (lambda (rat) (make-rational (- (numer rat)) (denom rat)))) 
 
;; add into complex package 
(put 'negate 'complex 
     (lambda (z) (make-from-real-imag (- (real-part z)) 
                                      (- (imag-part z))))) 
(put 'sub '(polynomial) (lambda (x y) (tag (add-poly x (negate y)))))

(put 'negate '(polynomial) negate-poly)

(define (negate-poly p) 
    (make-polynomial (variable p) 
                     (map 
                       (lambda (term) 
                         (make-term 
                           (order term) 
                           (negate (coeff term)))) 
                       (term-list p))))


     