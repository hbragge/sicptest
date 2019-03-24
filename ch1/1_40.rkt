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

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-trans g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton g guess)
  (fp (newton-trans g) guess))

(define (square x) (* x x))

(define (sqrt x)
  (newton (lambda (y) (- (square y) x))
          1.0))

(sqrt 4)