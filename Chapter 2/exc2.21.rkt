(define (square-list items)
    (if (null? items)
        null
        (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
    (map (lambda (x) (* x x)) items))