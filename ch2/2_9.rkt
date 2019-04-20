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
  (mul-int x (make-int (/ 1.0 (upperb y))
                       (/ 1.0 (lowerb y)))))

(define (width x)
  (/ (- (upperb x) (lowerb x)) 2))

(define z1 (make-int 1 2))
(define z2 (make-int 8 10))
(define z11 (make-int 3 4))
(define z22 (make-int 10 12))

(define x (mul-int z1 z1))
(display "x:")
(newline)
(width z1)
(width z2)
(width x)

(define x2 (mul-int z11 z11))
(display "x2:")
(newline)
(width z11)
(width z22)
(width x2)

(define s (add-int z1 z2))
(display "s:")
(newline)
(width z1)
(width z2)
(width s)

(define s2 (add-int z11 z22))
(display "s2:")
(newline)
(width z11)
(width z22)
(width s2)