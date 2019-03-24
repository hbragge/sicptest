#lang racket

(define (square x) (* x x))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (repeat (compose f f) (- n 1))))

((repeat square 2) 5)
