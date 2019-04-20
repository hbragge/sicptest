#lang racket

(define (make-int a b) (cons a b))

(define (upperb x) (car x))
(define (lowerb x) (cdr x))

(upperb (make-int 1 2))