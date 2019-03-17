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

(define (cont-fr n d k)
  (define (frac-rec i)
    (/ (n i)
       (+ (d i)
          (if (= i k)
              0
              (frac-rec (+ i 1))))))
  (frac-rec 1))

(define (cont-fri n d k)
  (define (iter k res)
    (if (= k 0)
        res
        (iter (- k 1) (/ (n k) (+ (d k) res)))))
  (iter k 0.0))

(define (tan-cf x k)
  (cont-fri (lambda (i)
              (if (= i 1)
                  x
                  (- (* x x))))
            (lambda (i) (- (* i 2) 1))
            k))

(tan-cf 0.785 21)