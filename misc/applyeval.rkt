#lang sicp

(define (app li el)
  (append li (list el)))

(define user-initial-environment (scheme-report-environment 5))

; modify expression and evaluate
(define a '(+ 1 2 3))
(define b (app a '4))

(eval b user-initial-environment)

; apply
(define (cube x) (expt x 3))
(apply cube '(2))