#lang racket
(define (cube-root x) (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x )
      guess
      (cube-root-iter (improve guess x) x)))

(define (improve guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (average x y)
  (/ (+ x y) 2))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(cube-root 8)

