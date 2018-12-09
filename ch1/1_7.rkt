#lang racket
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (define (good-enough? guess)
    (< (/ (abs (- (square guess) x)) x) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (cube x) (* x x x))
(define (avg3 x y) (/ (+ x y) 3))
(define (cubert x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (avg3 (/ x (square guess)) (* 2 guess)))
  (define (cubert-iter guess)
    (if (good-enough? guess)
        guess
        (cubert-iter (improve guess))))
  (cubert-iter 1.0))

(cubert 8)

;(sqrt 0.00012)
;(sqrt 2)
;(sqrt 16)