#lang racket

(define toler 0.0001)

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

(define (avg-damp f)
  (lambda (x) (avg x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

(define (nth n x)
  (if (= n 1)
      x
      (* x (nth (- n 1) x))))

(define (nroot n x)
  (fp ((repeat avg-damp (- n 1)) (lambda (y) (/ x (nth (- n 1) y))))
      1.0))

(nroot 5 32)
