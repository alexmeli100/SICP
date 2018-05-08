#lang racket

(define (expand-clauses clauses) 
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses))) 
            (if (cond-else-clause? first) 
                (if (null? rest) 
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF" clauses)) 
                (if (eq? (cadr first) '=>) 
                    (make-if (cond-predicate first) 
                             (list (caddr clause) (cond-predicate first))
                             (expand-clauses (rest)))
                    (make-if (cond-predicate first) 
                             (sequence->exp (cond-actions first))
                             (expand-clauses (rest))))))))