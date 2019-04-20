#lang racket

(define (make-int a b) (cons a b))

(define (upperb x) (cdr x))
(define (lowerb x) (car x))

(define (add-int x y)
  (make-int (+ (lowerb x) (lowerb y))
            (+ (upperb x) (upperb y))))

(define (sub-int x y)
  (make-int (- (lowerb x) (lowerb y))
            (- (upperb x) (upperb y))))

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

(define (width x)
  (/ (- (upperb x) (lowerb x)) 2))

(define z1 (make-int -1 2))
(define z2 (make-int 8 10))
(define z (div-int z2 z1))
(lowerb z)
(upperb z)