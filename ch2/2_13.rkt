#lang racket

(define (make-int a b) (cons a b))

(define (upperb x) (cdr x))
(define (lowerb x) (car x))

(define (make-cw c w)
  (make-int (- c w) (+ c w)))

(define (center i)
  (/ (+ (lowerb i) (upperb i)) 2))

(define (width i)
  (/ (- (upperb i) (lowerb i)) 2))

(define (make-cp c p)
  (make-int (* c (- 1.0 p)) (* c (+ 1.0 p))))

(define (percent i)
  (- (/ (upperb i) (center i)) 1.0))

(define (mul-int x y)
  (let ((p1 (* (lowerb x) (lowerb y)))
        (p2 (* (lowerb x) (upperb y)))
        (p3 (* (upperb x) (lowerb y)))
        (p4 (* (upperb x) (upperb y))))
    (make-int (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-int x y)
  (if (or (< (lowerb x) 0) (< (lowerb y) 0))
      (cons "error" "error")
      (mul-int x (make-int (/ 1.0 (upperb y))
                           (/ 1.0 (lowerb y))))))

(define i1 (make-cp 10 0.1))
(percent i1)

(define i2 (make-cp 10 0.2))
(percent i2)

(define i3 (mul-int i1 i2))
(lowerb i3)
(upperb i3)
(percent i3)