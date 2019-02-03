#lang racket

(define (even? n) (= (remainder n 2) 0)) 

(define (cube x) (* x x x)) 
  
(define (inc n) (+ n 1)) 
  
(define (sum term a next b) 
  (if (> a b) 
      0 
      (+ (term a) 
         (sum term (next a) next b)))) 

(define (simpson-integral f a b n) 
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(simpson-integral cube 0 1 1000)