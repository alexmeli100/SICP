#lang racket

(define (square-tree tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (* tree tree))
          (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree2 tree) 
    (map (lambda (x) 
            (if (pair? x) 
                (square-tree2 x)
                (* term term)))))

(define (tree-map proc tree)
    (map (lambda (x)
            (if (pair? x)
                (tree-map x)
                (proc x)))))
(define (square-tree3 tree)
    (define (square x) (* x x))
    (tree-map square tree))