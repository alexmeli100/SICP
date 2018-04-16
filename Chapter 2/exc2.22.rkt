#lang racket

(define (make-segment s e)
  (con s e))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (con x y))
(define (x-point x) (car x))
(define (y-point y) (cdr y))

(define (midpoint-segment segment)
  (let ((x1 (x-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y1 (y-point (start-segment segment)))
        (y2 (y-point (end-segment segment))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
)