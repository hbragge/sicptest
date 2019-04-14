#lang racket

(define (exp base n)
  (define (iter x result)
    ;; invariant: base^x * result is constant.
    (if (= 0 x)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))

(define (zerorem n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (exp divisor try-exp)))
        (iter (+ try-exp 1))  ;; Try another division.
        (- try-exp 1)))
  (iter 1))

(define (cons2 a b)
  (* (exp 2 a) (exp 3 b)))

(define (car2 z)
  (zerorem z 2))

(define (cdr2 z)
  (zerorem z 3))

(define x (cons2 0 2))
(car2 x)
(cdr2 x)

;(car2 (cons2 1 2))
;(cdr2 (cons2 1 2))
