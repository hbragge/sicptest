#lang racket

(define (make-int a b) (cons a b))

(define (upperb x) (cdr x))
(define (lowerb x) (car x))

(define (add-int x y)
  (make-int (+ (lowerb x) (lowerb y))
            (+ (upperb x) (upperb y))))

(define (sub-int x y)
  (make-int (- (lowerb x) (lowerb y))
            (- (upperb x) (upperb y))))

(define z (sub-int (make-int 4 6) (make-int 1 2)))
(car z)
(cdr z)
