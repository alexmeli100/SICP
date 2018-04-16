#lang racket

(define (index-of x tower) 
    (define (iter element tower result) 
        (cond ((null? tower) (error "element not found" element)) 
              ((eq? element (car tower)) result)
              (else (iter element (cdr tower) (+ result 1)))))
    (iter x tower 0))

(define tower '(integer rational real complex))

(define (raise-to-type x type tower) 
    (if (eq? type (car tower) x)
        (raise-to-type (raise x) type (cdr tower))))

(define (apply-generic op . args) 
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags))) 
            (if proc 
                (apply proc (map contents args)) 
                (if (= (length args) 2) 
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (< (index-of type1 tower) (index-of type2 tower))
                            (apply-generic op (raise-to-type a1 type2 tower) a2)
                            (apply-generic op a1 (raise-to-type a2 type1 tower))))
                    (error "No method for these types" (list op type-tags))))))