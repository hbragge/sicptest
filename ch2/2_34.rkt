#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;(((((1x + 0)x + 5)x + 0)x + 3)x + 1
;(+ (* ⟨higher-order terms⟩ x) ⟨coefficent⟩)

(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 2 (list 5 0 1))