#lang racket

(define (div-series s1 s2)
    (let ((c (stream-car s2))) 
        (if (= c 0) 
            (error "Constant term of denominator is 0" s2)
            (scale-stream (mul-series s1 (invert-unit-series (scale-stream s2 (/ 1 c)))) 
                          (/ 1 c)))))

(define tan-series (div-series sin-series cos-series))