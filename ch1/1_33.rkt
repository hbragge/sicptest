#lang racket

(define (sq x) (* x x)) 
  
(define (inc n) (+ n 1)) 

(define (id n) n)

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (acc comb nul term a next b)
  (if (> a b)
      nul
      (comb (term a)
            (acc comb nul term (next a) next b))))

(define (fiacc comb nul term a next b flt)
  (define (iter a res)
    (cond ((> a b) res)
          ((flt a) (iter (next a) (comb (term a) res)))
          (else (iter (next a) res))))
  (iter a nul))

(define (all n)
  true)

(define (sum term a next b)
  (fiacc + 0 term a next b all))

(define (sump term a next b)
  (fiacc + 0 term a next b prime?))

(sum sq 3 inc 5)
(sump sq 3 inc 5)
