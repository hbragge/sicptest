#lang racket

(define toler 0.00001)

(define (fp f first-guess)
  (define (close? v1 v2)
    (< (abs (- v1 v2)) toler))
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(fp (lambda (x) (+ 1 (/ 1 x)))
    1.0)