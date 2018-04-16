(define (up-split n)
    (if (= n 0)
        painter
        (let ((smaller (up-split (- n 1))))
            (below painter (beside smaller smaller)))))