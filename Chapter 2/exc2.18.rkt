#lang racket

(define (reverse items)
    (if (null? items)
        items
        (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 3 4 5 6))