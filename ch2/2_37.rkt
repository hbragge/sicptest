#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))
(map * (list 1 2 3) (list 4 5 6))

;(map + (list 1 2 3) (list 10 20 30) (list 100 200 300))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

(matrix-*-vector x (list 2 3 4 5))

(define (transpose m)
  (accumulate-n cons nil m))

(transpose (list (list 1 2) (list 3 4) (list 5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix x (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))) 
