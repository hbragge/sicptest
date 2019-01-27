#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (start-test n start-time)
  (display (smallest-divisor n))
  (report (- (runtime) start-time)))

(define (report elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-test n)
  (newline)
  (start-test n (runtime)))

(timed-test 472882049)