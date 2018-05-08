#lang racket

(define (evaln expr env) 
    (cond ((self-evaluating? expr) expr) 
          ((variable? expr) (lookup-variable-value expr env)) 
          ((get 'op (car expr)) (get 'op (car expr) expr env)) 
          ((application? expr)  
           (applyn (evaln (operator expr) env)  
                          (list-of-values (operands expr) env))) 
          (else (error "Unknown expression type -- EVAL" expr))))   