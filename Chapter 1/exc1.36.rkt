#lang racket

(define tolerance 0.00001)

(define (fixed-point f fixed-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try fixed-guess))

(define (x-to-the-x y)
  (fixed-point (lambda (x) (/ (log y) (log x))) 10.0))

# with average damping
(define (x-to-the-x y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 10.0))