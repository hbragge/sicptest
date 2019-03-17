#lang racket

(define (sq x) (* x x)) 
  
(define (inc n) (+ n 1)) 
  
(define (prod term a next b) 
  (if (> a b) 
      1 
      (* (term a) 
         (prod term (next a) next b)))) 

(define (iprod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (id x) x)

(define (fact n)
  (iprod id 1 inc n))

;(prod sq 2 inc 3)
;(iprod sq 2 inc 3)
;(fact 5)

;pi = 2 4 4 6 6 8 8
;4    3 3 5 5 7 7 9

(define (even? n) (= (remainder n 2) 0))

(define (wallis n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 3))
        (/ (+ k 3) (+ k 2))))
  (* 4 (iprod term 0 inc n)))

(exact->inexact (wallis 10))
(exact->inexact (wallis 100))
(exact->inexact (wallis 1000))