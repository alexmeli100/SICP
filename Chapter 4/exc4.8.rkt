#lang racket

;;(define (let? exp) (tagged-list? (car exp) 'let))

(define (let-body exp) (cddr exp))

(define (let-vars exp) (map car (cadr exp)))

(define (let-exps exp) (map cadr (cadr exp)))

(define (make-define var-name value) 
    (cons 'define (list var-name value)))

(define (make-funcall var-name params) (list (cons var-name params)))

(define (let->combination exp) 
    (cond ((not (pair? (cadr exp))) 
                (let ((inits (map car (caddr exp)))
                      (values (map cadr (caddr exp)))
                      (body (cdddr exp))
                      (var-name (cadr exp))) 
                    (let* ((lambda-body (cons (make-define var-name 
                                                          (make-lambda inits body))
                                             (make-funcall var-name inits)))
                          (proc (make-lambda inits lambda-body)))
                        (cons proc values))))
          (else (let ((proc (make-lambda (let-vars exp)
                      (let-body exp))))
                    (cons proc (let-exps exp))))))

(define (make-lambda parameters body) 
    (cons 'lambda (cons parameters body)))


