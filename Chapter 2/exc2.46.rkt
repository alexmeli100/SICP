#lang racket

(define (make-vect x y) (cons x y))

(define (xcor-vect vector) (car vector))

(define (ycor-vect vector) (cdr vector))

(define (add-vect vector1 vector2) 
    (let ((xcor (+ (xcor-vect vector1) (xcor-vect vector2)))
          (ycor (+ (ycor-vect vector1) (ycor-vect vector2))))
        (make-vect xcor ycor)))

(define (sub-vect vector1 vector2) 
    (let ((xcor (- (xcor-vect vector1) (xcor-vect vector2)))
          (ycor (- (ycor-vect vector1) (ycor-vect vector2))))
        (make-vect xcor ycor)))

(define (scale-vect vector scalar)
    (make-vect (* scalar (xcor-vect vector)) (* scalar (ycor-vect vector))))