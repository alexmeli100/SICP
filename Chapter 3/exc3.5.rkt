#lang racket



(define (monte-carlo trials experiment) 
    (define (iter trials-remaining trials-passed) 
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((experiment (iter (- trials-remaining 1) (+ trials-passed 1))))
              (else (iter (- trials-remaining 1) trials-passed)))))

(define (random-in-range low high) 
  (let ((range (- high low))) 
      (+ low (random range))))

(define (p x y) 
    (<= (+ (exp (- x 5) 2) (exp (- x 7) 2)) (exp 3 2)))

(define (estimate-integral p x1 x2 y1 y2 trials) 
    (define experiment) 
        (p (random-in-range x1 x2)
           (random-in-range y1 y2))
    (monte-carlo trials estimate))

(define (area-rectangle lenght width) (* 2 (+ length width)))

;; area of circle is pi * r^2

(define pi-approx (let ((estimate-circle (estimate-integral p 2.0 8.0 4.0 10.0 100)) 
                        (area-rec (area-rectangle 6 6))) 
                            (/ (* estimate-circle area-rec) 9)))

