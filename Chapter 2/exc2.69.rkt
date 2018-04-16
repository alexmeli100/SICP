#lang racket

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs) 
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves) 
    (if (null? (cdr leaves))
        (car leaves)
        (adjoin-set (make-code-tree (car leaves) (cadr leaves)) (cddr leaves))))