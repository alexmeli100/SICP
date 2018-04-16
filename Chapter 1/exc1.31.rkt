#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)

(define (factorial n) 
  (product-iter identity 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) 
        result 
        (iter (next a) (* result (term a)))))
  (iter a 1))

(factorial 4)