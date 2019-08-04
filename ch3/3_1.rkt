#lang racket

(define (make-acc value)
  (lambda (inc)
    (begin (set! value (+ value inc))
           value)))

(define a (make-acc 5))
(a 10)
(define b (make-acc 50))
(b 15)
(a 10)