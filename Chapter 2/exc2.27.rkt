#lang racket

(define (deep-reverse items)
    (if (pair? items)
        (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
        items))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
