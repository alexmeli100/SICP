#lang racket

(define (solve-2nd y0 dy0 a b dt) 
    (define y (integral (delay dy) y0 dt)
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (add-streams (scale-stream dy a) (scale-stream y b))))
    y)