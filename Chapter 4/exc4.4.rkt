#lang racket

(define (eval exp env) 
    (cond ...
          ...

          ((and? exp) (eval-and exp env))
          ((or? exp) (eval-or env))

          ...
          ...))

(define (or? exp) 
    (tagged-list? exp 'or))

(define (or-expressions exp) 
    (cdr exp))

(define (eval-or exp env) 
    (let ((expressions (or-expressions exp))))
    (define (helper expressions env) 
        (if (null? expressions)
            false
            (let ((result (eval (car expressions) env))) 
                (if (true? result)
                    result
                    (helper (cdr expressions) env)))))
    (helper expressions env))

(define (and? exp) 
    (tagged-list? exp 'and))

(define (and-expressions exp) 
    (cdr exp))

(define (eval-and exp env) 
    (let ((expressions (and-expressions exp))))
    (define (helper expressions env) 
        (cond ((null? expressions) true) 
              ((null? (cdr expressions)) 
                      (let ((result (eval (car expressions) env))) 
                        (if (true? result)
                            result
                            false)))
              ((true? (eval (car expressions) env)) (helper (cdr exp) env))
              (else false)))
    (helper expressions env))


;; solution using derived expressions

(define (and->if exp) 
    (expand-and-clauses (and-expressions exp)))

(define (expand-and-clauses clauses) 
    (if (null? clauses) 
        'true
        (if (null? (cdr clauses))
            (make-if (car clauses)
                     (car clauses)
                     'false)
            (let ((first (car clauses))
                  (rest (cdr clauses)))
                (make-if (first)
                         (expand-and-clauses rest)
                         'false))))
)

(define (or-if exp) 
    (expand-or-clauses (or-expressions exp)))

(define (expand-or-clauses clauses) 
    (if (null? clauses) 
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (make-if (first)
                     (first)
                     (expand-or-clauses rest)))))