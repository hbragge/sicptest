#lang racket

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n ))


(define (fast-expt2 b n)
  (define (fast-expt-it b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-it b (/ n 2) (* a (fast-expt-it b (/ n 2) 1))))
          (else (fast-expt-it b (- n 1) (* b a)))))
  (fast-expt-it b n 1))

(fast-expt 2 8)
(fast-expt2 2 8)
