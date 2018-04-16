#lang racket

(define (make-table same-key?) 
    (let ((local-table (list 'table)))

        (define (lookup keys) 
            (define (look keys table) 
                (let ((subtable (assoc (car keys) (cdr table)))) 
                    (if subtable 
                        (if (null? (cdr keys)) 
                            (cdr subtable)
                            (look (cdr keys) subtable))
                        false)))
            (look keys local-table))

            (define (insert! table keys value) 
                (define (insert-helper remaining-keys last-found) 
                  (if (null? (cdr remaining-keys)) 
                      ;;; when (cdr remaining-keys) is null, we should update 
                      ;;; or create a record 
                      (let ((record (assoc (car remaining-keys) (cdr last-found)))) 
                        (if record 
                            (set-cdr! record value) 
                            (set-cdr! last-found 
                                      (cons (cons (car remaining-keys) value) 
                                            (cdr last-found))))) 
                      ;;; when (cdr remaining-keys) is not null, we should find 
                      ;;; or create tables 
                      (let ((next-table (assoc (car remaining-keys) (cdr last-found)))) 
                        (if next-table 
                            (insert-helper (cdr remaining-keys) next-table) 
                            (let ((new-table (list (car remaining-keys)))) 
                              (set-cdr! last-found 
                                        (cons new-table 
                                              (cdr last-found))) 
                              (insert-helper (cdr remaining-keys) new-table)))))) 
                (insert-helper keys table) 
              'ok) 
              
        (define (dispatch m) 
            (cond ((eq? m 'lookup-proc) lookup) 
                  ((eq? m 'insert-proc) insert!)
                  (else (error "Unknown operation -- TABLE" m)))) 
        (define (assoc key records) 
            (cond ((null? records) false) 
                  ((same-key? key (caar records)) (car records)) 
                  (else (assoc key (cdr records)))))
        dispatch))


    