#lang racket

(define (filtered-accumulate combiner null-value term a next b filter)
  (cond (( > a b) null-value)
        ((filter (term a)) (combiner (term a) (filtered-accumulate accumulate combiner null-value term (next a ) next b)))
        (else (filtered-accumulate combiner null-value term (next a ) next b filter))))