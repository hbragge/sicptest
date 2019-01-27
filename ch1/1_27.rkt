#lang racket
(require racket/trace)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n a)
  (= (expmod a n n) a))

(define (fast-prime? n a)
  (cond ((= a n) true)
        ((fermat-test n a) (fast-prime? n (+ a 1)))
        (else false)))

(define (fprime? n)
  (fast-prime? n 2))

;(trace expmod)
(fprime? 8911)