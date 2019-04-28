#lang racket

(define (last-pair l)
  (if (< (length l) 2)
      l
      (last-pair (cdr l))))

(define (before-last l)
  (if (< (length l) 2)
      '()
      (cons (car l) (before-last (cdr l)))))

(define (reverse l)
  (if (< (length l) 2)
      l
      (cons (car (last-pair l)) (reverse (before-last l)))))

(reverse (list 1 4 9 16 25))
