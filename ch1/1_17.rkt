#lang racket

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (fmul a b)
  (cond ((= b 0) 0)
        ((even? b) (fmul (double a) (halve b)))
        (else (+ a (fmul a (- b 1))))))

; ex 1.18
(define (fmul2 a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter a (halve b) (+ acc (* a (halve b)))))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

(mul 2 10)
(fmul 2 10)
(fmul2 2 10)