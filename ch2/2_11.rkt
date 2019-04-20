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

; patt |  min  |  max 
; ++++ | al bl | ah bh 
; ++-+ | ah bl | ah bh 
; ++-- | ah bl | al bh 
; -+++ | al bh | ah bh 
; -+-+ | trouble case 
; -+-- | ah bl | al bl 
; --++ | al bh | ah bl 
; ---+ | al bh | al bl 
; ---- | ah bh | al bl
(define (mul-int2 x y) 
  (let ((x1 (lowerb x)) 
        (x2 (upperb x)) 
        (y1 (lowerb y)) 
        (y2 (upperb y))) 
    (let ((x-neg (< x2 0)) 
          (x-pos (> x1 0)) 
          (y-neg (< y2 0)) 
          (y-pos (> y1 0))) 
      (cond (x-neg (cond (y-neg (make-int (* x2 y2) (* x1 y1))) 
                         (y-pos (make-int (* x1 y2) (* x2 y1))) 
                         (else (make-int (* x1 y2) (* x1 y1))))) 
            (x-pos (cond (y-neg (make-int (* x2 y1) (* x1 y2))) 
                         (y-pos (make-int (* x1 y1) (* x2 y2))) 
                         (else (make-int (* x2 y1) (* x2 y2))))) 
            (else (cond (y-neg (make-int (* x2 y1) (* x1 y1))) 
                        (y-pos (make-int (* x1 y2) (* x2 y2))) 
                        (else (make-int (min (* x1 y2) (* x2 y1)) 
                                             (max (* x1 y1) (* x2 y2))))))))))

(define (div-int x y)
  (if (or (< (lowerb x) 0) (< (lowerb y) 0))
      (cons "error" "error")
      (mul-int x (make-int (/ 1.0 (upperb y))
                           (/ 1.0 (lowerb y))))))

(define (width x)
  (/ (- (upperb x) (lowerb x)) 2))

(define z1 (make-int -2 -1))
(define z2 (make-int 8 10))
(define m (mul-int z2 z1))
(lowerb m)
(upperb m)

(define m2 (mul-int2 z2 z1))
(lowerb m2)
(upperb m2)