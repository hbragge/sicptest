#lang racket

(define (count-change amount) 
  (cc amount 5)) 
(define (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (= kinds-of-coins 0)) 0) 
        (else (+ (cc amount 
                     (- kinds-of-coins 1)) 
                 (cc (- amount 
                        (first-denomination kinds-of-coins)) 
                     kinds-of-coins))))) 
(define (first-denomination kinds-of-coins) 
  (cond ((= kinds-of-coins 1) 50) 
        ((= kinds-of-coins 2) 25) 
        ((= kinds-of-coins 3) 10) 
        ((= kinds-of-coins 4) 5) 
        ((= kinds-of-coins 5) 1)))

(count-change 100)