#lang racket

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define eu-coins (list 200 100 50 20 10 5 2 1))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? kinds-of-coins)) 0)
        (else (+ (cc amount
                     (cdr kinds-of-coins))
                 (cc (- amount (car kinds-of-coins))
                     kinds-of-coins)))))

(cc 100 us-coins)
(cc 100 uk-coins)
(cc 100 eu-coins)