#lang racket

(define dx 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smoothn f n)
  ((repeat smooth n) f))

(define (sq x) (* x x))

((smooth sq) 2)
((smoothn sq 2) 2)