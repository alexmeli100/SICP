#lang racket

(define (triples s t u) 
    (cons-stream 
        (list (stream-car s) (stream-car t) (stream-car u)) 
        (interleave 
            (interleave 
                (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) 
                            (stream-cdr u))
                (stream-map (lambda (x) (list (stream-car s) (stream-cadr t) x)) 
                            (stream-cdr u))) 
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples (stream-filter (lambda (triple) 
                                               (let ((x (square (stream-car triple)))
                                                     (y (square (stream-cadr triple)))
                                                     (z (square (stream-caddr triple))))
                                                    (if (= (+ x y) z)
                                                        true
                                                        false))) int-triples))

(define int-triples (triples integers integers integers))

