#lang racket
; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; 1.3
(define (sq x) (* x x))

(define (f a b c)
  (cond ((and (< a b) (< a c)) (+ (sq b) (sq c)))
        ((and (< b a) (< b c)) (+ (sq a) (sq c)))
        (else (+ (sq a) (sq b)))))
(f 1 2 3)

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 1 -2)

; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))
