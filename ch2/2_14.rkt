#lang racket

; 2.14-2.16

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

(define (add-int x y)
  (make-int (+ (lowerb x) (lowerb y))
            (+ (upperb x) (upperb y))))

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

(define (par1 r1 r2)
  (div-int (mul-int r1 r2)
           (add-int r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-int 1 1)))
    (div-int one
             (add-int (div-int one r1)
                      (div-int one r2)))))

(define i1 (make-cp 10 0.1))
(define i2 (make-cp 20 0.2))
(define res1 (par1 i1 i2))
;(lowerb res1)
;(upperb res1)

(define res2 (par2 i1 i2))
;(lowerb res2)
;(upperb res2)

(define res3 (div-int i1 i1))
(lowerb res3)
(upperb res3)
(percent res3)
(newline)
