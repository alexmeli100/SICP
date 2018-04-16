#lang racket

(define (triples n s)
    (filter (lambda (triple)
                (= value (accumulate + 0 triple)))
            (unique-triples n)))

(define (unique-triples n)
    (flatmap (lambda (i)
                (map (lambda (j)
                    (map (lambda (k) (list i j k))
                        (enumerate-interval 1 ( - j 1))))
                (enumerate-interval 1 (- i 1)))
             (enumerate-interval 1 n))))
