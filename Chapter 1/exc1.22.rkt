#lang racket 

(define (square x) (* x x)) 
  
    (define (smallest-divisor n) 
      (find-divisor n 2)) 
     
    (define (find-divisor n test-divisor) 
      (cond ((> (square test-divisor) n) n) 
            ((divides? test-divisor n) test-divisor) 
            (else (find-divisor n (next test-divisor))))) 
    
    (define (next n)
      (if (= n 2) 3 (+ n 2)))
     
    (define (divides? a b) 
      (= (remainder b a) 0)) 
     
    (define (prime? n) 
      (= n (smallest-divisor n))) 
     
    (define (timed-prime-test n) 
      (start-prime-test n (current-milliseconds))) 
     
    (define (start-prime-test n start-time) 
      (when (prime? n) 
          (report-prime n (- (current-milliseconds) start-time)))) 
     
    (define (report-prime n elapsed-time) 
      (newline) 
      (display n) 
      (display " *** ") 
      (display elapsed-time)) 
     
    (define (search-for-primes first last) 
      (define (search-iter cur last) 
        (when (<= cur last) (timed-prime-test cur)) 
        (when (<= cur last) (search-iter (+ cur 2) last))) 
      (search-iter (if (even? first) (+ first 1) first) 
                   (if (even? last) (- last 1) last))) 
     
    (search-for-primes 1000 1019)       
    (search-for-primes 10000 10037)     
    (search-for-primes 100000 100043)   
    (search-for-primes 1000000 1000037) 