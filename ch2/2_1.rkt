#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (make-rat-abs (/ n g) (/ d g))))

(define (make-rat-abs n d)
  (cond ((and (< n 0) (< d 0)) (cons (abs n) (abs d)))
        ((and (>= n 0) (< d 0)) (cons (- n) (abs d)))
        (else (cons n d))))

(define (print x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))

(print (make-rat 3 9))
(print (make-rat -2 4))
(print (make-rat 1 -8))
(print (make-rat -5 -9))