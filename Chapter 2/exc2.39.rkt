(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x)) nil sequence))

;; reverse using left fold
(define (reverse2 sequence)
    (fold-left (lambda (result first) (cons first result)) null sequence))