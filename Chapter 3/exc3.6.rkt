(define rand 
    (let ((x random-init)) 
      (define (dispatch message) 
        (cond ((eq? message 'generate) 
                (begin (set! x (rand-update x)) 
                       x)) 
              ((eq? message 'reset) 
                (lambda (new-value) (set! x new-value))))) 
      dispatch)) 