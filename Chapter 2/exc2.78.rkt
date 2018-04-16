#lang racket

(define (attach-tag type-tag contents) 
    (if (eq? type-tag 'scheme-number) 
        contents 
        (cons type-tag contents))) 
   
(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum)) 
        (else (error "Wrong datum -- TYPE-TAG" datum)))) 
 
(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum)) 
        (else (error "Wrong datum -- CONTENTS" datum))))