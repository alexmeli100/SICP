#lang racket

(define (element-of-set? x set) 
    (cond ((null? set) false) 
          ((equal? x (car set)) true) 
          (else (element-of-set? x (cdr set))))) 

(define (make-leaf symbol weight) 
    (list 'leaf symbol weight))

(define (leaf? object) 
    (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
    (list left 
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree) 
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree) 
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree) 
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE BRANCH" bit))))

(define (encode message tree) 
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

(define (encode-symbol symbol tree) 
    (define (encode symbol tree)
        (let ((left (left-branch tree)) (right (right-branch tree))) 
            (cond ((leaf? tree) '())
                  ((element-of-set? symbol (symbols left)) (cons 0 (encode symbol left)))
                  ((element-of-set? symbol (symbols right)) (cons 1 (encode symbol right))))))
    (if (not (element-of-set? symbol (symbols tree)))
        (error "bad symbol -- CHOOSE BRANCH" symbol)
        (encode symbol tree)))

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
        (successive-merge (adjoin-set (make-code-tree (car leaves) (cadr leaves)) (cddr leaves)))))
(define pairs '((a 2) (na 16) (boom  1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1)))
(define huff-tree (generate-huffman-tree pairs))

(define song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)) 

(encode song huff-tree)