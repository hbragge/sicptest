#lang sicp

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime n)
  (if (= (smallest-divisor n) n)
      1
      0))

(define (start-prime-test n start-time)
  (display (prime n))
  (report-prime (- (runtime) start-time)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (check-for-primes n)
  (define (end-test total start-time)
    (newline)
    (display "primes found: ")
    (display total)
    (newline)
    (display "runtime: ")
    (display (- (runtime) start-time)))
  (define (iter i total start-time)
    (if (= total 3)
        (end-test total start-time)
        (iter (+ i 1) (+ total (prime i)) start-time)))
  (iter n 0 (runtime)))

;(timed-prime-test 1999)
(check-for-primes 100000000000)
(check-for-primes 1000000000000)
(check-for-primes 10000000000000)