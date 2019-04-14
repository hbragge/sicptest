#lang racket

;HARD

(define zero (lambda (f) (lambda (x) x))) 

(define (add1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add2 n)
  (lambda (f) (lambda (x) (f (f ((n f) x))))))
;  (add1 (add1 n)))

(define (int2c n)
  (if (= n 0)
      zero
      (add1 (int2c (- n 1)))))

(define (c2int cn)
  ((cn (lambda (n) (+ n 1))) 0))

(c2int zero)
(c2int (add1 zero))
(c2int (add2 zero))
(c2int (add2 (add1 zero)))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

