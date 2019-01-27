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

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

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
        (iter (+ i 1) (+ total (fprime i)) start-time)))
  (iter n 0 (runtime)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime n times)
  (cond ((= times 0) 1)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else 0)))

(define (fprime n)
  (fast-prime n 10))

(check-for-primes 10000000)
(check-for-primes 10000000)
(check-for-primes 1000000000)
(check-for-primes 1000000000)
;(check-for-primes 10000000000000)
