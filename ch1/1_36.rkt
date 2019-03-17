#lang racket

(define toler 0.001)

(define (fp f first-guess)
  (define (close? v1 v2)
    (< (abs (- v1 v2)) toler))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close? guess next)
          next
          (try next))))
  (try first-guess))

(define (avg a b)
  (/ (+ a b) 2))

(fp (lambda (x) (avg x (/ (log 1000) (log x))))
    2.0)

;(define (sqrt x)
;  (fp (lambda (y) (avg y (/ x y)))
;      1.0))

;(sqrt 4)