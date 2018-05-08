#lang racket

(define (let? expt) (tagged-list? (car exp) 'let))

(define (let-body exp) (cddr exp))

(define (let-vars exp) (map car (cadr exp)))

(define (let-exps exp) (map cadr (cadr exp)))

(define (let->combination exp) 
    (let ((proc (make-lambda (let-vars exp)
                             (let-body exp))))
        (cons proc (let-exps exp))))

(define (make-lambda parameters body) 
    (cons 'lambda (cons parameters body)))

(let->combination '(let (('x 3) ('y 5)) (+ 'x 'y)))
