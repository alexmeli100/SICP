(define (stream-map proc . argstreams) 
    (if (null-stream? (car atgstreams) 
        the-empty-stream
        (cons-stream 
            (apply proc (map stream-car argstream))
            (apply stream-map 
                   (cons proc (map stream-cdr arguments)))))))