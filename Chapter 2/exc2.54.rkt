#lang racket

(define (equal? list1 list2) 
    (cond ((and (null? list1) (null? list2)) true)
          ((and (and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2)) true)
          ((and (pair? list1) (pair? list2)) (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
          (else false)))

(define (equal2? list1 list2)
    (or
        (eq? list1 list2)
        (and
            (pair? list1)
            (pair? list2)
            (equal? (car list1) (car list2))
            (equal? (cdr list1) (cdr list2)))))

(equal2? '(this (is a)) '(this (is a)))