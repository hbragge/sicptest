#lang sicp

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define vs (make-vect 1 1))
(define ve (make-vect 2 2))

(define s1 (make-segment vs ve))
(start-segment s1)
(end-segment s1)