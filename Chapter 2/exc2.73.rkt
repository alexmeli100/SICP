(define (install-symbolic-differentiation-package) 
    
    (define (addend s) (car s)) 
   
    (define (augend s) 
      (let ((cs (cdr s))) 
        (if (null? (cdr cs)) 
            (car cs) 
            (cons '+ cs)))) 
     
    (define (make-sum a1 a2) 
      (cond ((=number? a1 0) a2) 
            ((=number? a2 0) a1) 
            ((and (number? a1) (number? a2))  
             (+ a1 a2)) 
            (else (list '+ a1 a2)))) 
     
    (put 'deriv '+ (lambda (operands var) 
                     (make-sum (deriv (addend operands) var) 
                                (deriv (augend operands) var)))) 
   
    (define (multiplier p) (car p)) 
    
    (define (multiplicand p) 
      (let ((cs (cdr p))) 
        (if (null? (cdr cs)) 
            (car cs) 
            (cons '* cs)))) 
   
    (define (make-product m1 m2) 
      (cond ((or (=number? m1 0)  
                 (=number? m2 0))  
             0) 
            ((=number? m1 1) m2) 
            ((=number? m2 1) m1) 
            ((and (number? m1) (number? m2))  
             (* m1 m2)) 
            (else (list '* m1 m2)))) 
     
    (put 'deriv '* (lambda (operands var) 
                     (make-sum 
                      (make-product  
                       (multiplier operands) 
                       (deriv (multiplicand operands) var)) 
                      (make-product  
                       (deriv (multiplier operands) var) 
                       (multiplicand operands))))) 
     
    (define base car) 
    (define exponent cadr) 
     
    (define (make-exponentiation base exponent) 
      (cond ((=number? base 0) 0) 
            ((=number? exponent 1) base) 
            ((=number? exponent 0) 1) 
            ((and (number? base) (number? exponent))  
             (expt base exponent)) 
            (else (list '** base exponent)))) 
     
    (put 'deriv '** (lambda (operands var) 
                     (make-product (exponent operands) 
                                   (make-product (make-exponentiation (base operands) 
                                                                      (- (exponent operands) 1)) 
                                                 (deriv (base operands) var))))) 
     
    'done) 
   
  (define (variable? x) (symbol? x)) 
   
  (define (same-variable? v1 v2) 
    (and (variable? v1) 
         (variable? v2) 
         (eq? v1 v2))) 
   
  (define (=number? exp num) 
    (and (number? exp) (= exp num))) 
   
  (define (deriv exp var) 
     (cond ((number? exp) 0) 
           ((variable? exp)  
             (if (same-variable? exp var)  
                 1  
                 0)) 
           (else ((get 'deriv (operator exp))  
                  (operands exp)  
                  var)))) 
   
  (define (operator exp) (car exp)) 
  (define (operands exp) (cdr exp)) 