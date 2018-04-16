#lang racket

(define (same-parity x . y)
    (define z (remainder x 2))
    (define (parity items)
        (if (null? items)
            items
            (if (= (remainder (car items) 2) z)
                (cons (car items) (parity (cdr items)))
                (parity (cdr items)))))
    (cons x (parity y)))

    (same-parity 1 2 3 4 5 6 7)