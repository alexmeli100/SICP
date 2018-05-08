#lang racket 

 ;; a Assignment expressions are technically pairs and will be evaluated as applications. Evaluating an assignment as an application will cause the evaluator to try to evaluate the assignment variable instead of treating it as a symbol. 
  
 ;; b 
 (define (application? exp) (tagged-list? exp 'call)) 
    (define (operator exp) (cadr exp)) 
    (define (operands exp) (cddr exp)) 