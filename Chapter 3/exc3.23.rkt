#lang racket

;; deque (double-ended queue) implementation

(define (make-deque) (cons nil nil)) 
    (define (front-ptr deque) (car deque)) 
    (define (rear-ptr deque) (cdr deque)) 
    (define (empty-deque? deque) (null? (front-ptr deque))) 
    (define (set-front! deque item) (set-car! deque item)) 
    (define (set-rear! deque item) (set-cdr! deque item)) 
     
    (define (get-item deque end) 
      (if (empty-deque? deque) 
        (error "Trying to retrieve item from empty deque" deque) 
        (caar (end deque)))) 
     
    (define (insert-deque! deque item end) 
      (let ((new-pair (cons (cons item nil) nil))) 
        (cond ((empty-deque? deque) 
               (set-front! deque new-pair) 
               (set-rear! deque new-pair)) 
              ((eq? end 'front) 
               (set-cdr! new-pair (front-ptr deque)) 
               (set-cdr! (car (front-ptr deque)) new-pair) 
               (set-front! deque new-pair)) 
              (else (set-cdr! (rear-ptr deque) new-pair) 
                    (set-cdr! (car new-pair) (rear-ptr deque)) 
                    (set-rear! deque new-pair))))) 
     
    (define (front-delete-deque deque) 
      (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque)) 
            (else (set-front! deque (cdr (front-ptr deque))) 
                  (or (empty-deque? deque) (set-cdr! (car (front-ptr deque)) nil))))) 
     
    (define (rear-delete-deque deque) 
      (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque)) 
            (else (set-rear! deque (cdar (rear-ptr deque))) 
                  (if (null? (rear-ptr deque)) (set-front! deque nil) 
                    (set-cdr! (rear-ptr deque) nil))))) 
     
    (define (front-insert-deque! deque item) (insert-deque! deque item 'front)) 
    (define (rear-insert-deque! deque item) (insert-deque! deque item 'rear)) 
    (define (front-deque deque) (get-item deque front-ptr)) 
    (define (rear-deque deque) (get-item deque rear-ptr)) 
     
    (define (print-deque d) 
      (define (iter res _d) 
        (if (or (null? _d) (empty-deque? _d)) res 
          (iter (append res (list (caaar _d))) (cons (cdar _d) (cdr d))))) 
      (iter nil d)) 