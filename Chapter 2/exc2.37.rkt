#lang racket

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
    (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x)
                (accumulate (lambda (y z) 
                    (cons (dot-product x y) z)) null cols)) m)))

