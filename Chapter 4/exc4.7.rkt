#lang racket

(define (let*-exp exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (make-let init body) 
    (list 'let (list init) body))

(define (let*->nested-lets exp) 
    (let ((expressions (let*-exp exp)) 
          (body (let*-body exp)))
        (define (helper expressions)
            (if (null? (cdr expressions)) 
                (make-let (car expressions) body)
                (make-let (car expressions) (helper (cdr expressions))))) 
        (helper expressions)))

(let*->nested-lets '(let (('x 3) ('y (+ 'x 2))) (+ 'y 6)))
