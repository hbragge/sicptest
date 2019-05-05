#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (* x x)) (list 1 2 3))
(map2 (lambda (x) (* x x)) (list 1 2 3))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))
(append2 (list 1 2 3) (list 4 5 6))

(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 2 3 4 5))
(length2 (list 2 3 4 5))