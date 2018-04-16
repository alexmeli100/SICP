#lang racket

;; a
(define (integrate-series stream) 
    (mul-streams stream rational-coeffs))

(define (div-streams s1 s2) 
    (stream-map / s1 s2))

(define (mul-streams s1 s2) 
    (stream-map * s1 s2))

(define rational-coeffs (div-streams ones integers))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))