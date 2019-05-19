#lang sicp

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w)) (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w)) (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define v1 (make-vect 1 2))

(define v2 (add-vect v1 v1))
(xcor-vect v2)
(ycor-vect v2)

(define v3 (sub-vect v2 v1))
(xcor-vect v3)
(ycor-vect v3)

(define v4 (scale-vect 10 v1))
(xcor-vect v4)
(ycor-vect v4)