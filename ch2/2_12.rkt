#lang racket

(define (make-int a b) (cons a b))

(define (upperb x) (cdr x))
(define (lowerb x) (car x))

(define (make-cw c w)
  (make-int (- c w) (+ c w)))

(define (center i)
  (/ (+ (lowerb i) (upperb i)) 2))

(define (width i)
  (/ (- (upperb i) (lowerb i)) 2))

(define (make-cp c p)
  (make-int (* c (- 1.0 p)) (* c (+ 1.0 p))))

(define (percent i)
  (- (/ (upperb i) (center i)) 1.0))

(define i1 (make-cp 10 0.1))
(percent i1)
