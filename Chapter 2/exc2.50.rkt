#lang racket

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-cord-map frame)))
            (let ((new-origin (m origin)))
                (painter (make-frame new-origin 
                                     (sub-vect (m corner1) new-origin)
                                     (sub-vect (m corner2) new-origin)))))))
;; flip painter horizontally
(define (flip-horiz painter)
    (transform-painter painter
                       (make-vector 1.0 0.0)
                       (make-vector 0.0 0.0)
                       (make-vector 1.0 1.0)))
;; rotate painter by 180 degrees counterclockwise
(define (rotate180 painter)
    (transform-painter painter 
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

;; rotate painter by 270 degrees counterclockwise
(define (rotate270 painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 1.1 1.1)
                       (make-vect 0.0 0.0)))
