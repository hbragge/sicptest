#lang racket

(define toler 0.00001)

(define (close? v1 v2)
  (< (abs (- v1 v2)) toler))

(define (iter-imp good? imp)
  (lambda (guess)
    (let ((next (imp guess)))
      (display guess)
      (newline)
      (if (good? guess next)
          next
          ((iter-imp good? imp) next)))))

(define (avg a b)
  (/ (+ a b) 2))

(define (avg-damp f)
  (lambda (x) (avg x (f x))))

(define (sqrt x)
  ((iter-imp close?
             (avg-damp (lambda (y) (/ x y))))
   1.0))

(define (fp f first-guess)
  ((iter-imp close? f) first-guess))

;(sqrt 4)

(fp (lambda (x) (+ 1 (/ 1 x)))
    1.0)