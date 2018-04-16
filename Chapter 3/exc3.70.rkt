#lang racket

(define (merge-weighted s1 s2 weight) 
    (cond ((stream-null? s1) s2) 
          ((stream-null? s2) s1)
          (else 
              (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< (weight s1car) (weight s2car)) (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))) 
                      ((> (weight s1car) (weight s2car)) (cons-stream s2car (merge-weighted s1 (stream-cdr s2 weight))))
                      (else 
                        (cons-stream s1car (cons-stream s2car (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))))

(define (weighted-pairs s1 s2 weight)
(cons-stream (list (stream-car s1) (stream-car s2))
                (merge-weighted (stream-map (lambda (x) (list (stream-car s1) x))
                                                                   (stream-cdr s2))
                                         (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
                                                         weight)))

(define (weight1 x) (+ (car x) (cadr y)))

;;a
(define pairs1 (weighted-pairs integers integers weight1))


(define weight2 (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x)))))   
   (define (divide? x y) (= (remainder y x) 0))
   (define stream235
           (stream-filter (lambda (x) (not (or (divide? 2 x) (divide? 3 x) (divide? 5 x))))
                                      integers))
   (define pairs2 (weighted-pairs stream235 stream235 weight2))
