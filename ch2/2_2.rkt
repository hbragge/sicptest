#lang racket

(define (make-p x y)
  (cons x y))

(define (print-p p)
  (newline)
  (display "(")
  (display (x-co p))
  (display ",")
  (display (y-co p))
  (display ")"))

(define (x-co p)
  (car p))

(define (y-co p)
  (cdr p))

(define (make-s start end)
  (cons start end))

(define (start-s seg)
  (car seg))

(define (end-s seg)
  (cdr seg))

(define (avg a b)
  (/ (+ a b) 2.0))

(define (mid-p seg)
  (let ((start-x (x-co (start-s seg)))
        (start-y (y-co (start-s seg)))
        (end-x (x-co (end-s seg)))
        (end-y (y-co (end-s seg))))
    (make-p (avg start-x end-x) (avg start-y end-y))))

(define seg1 (make-s (make-p 1 2) (make-p 3 6)))
(print-p (mid-p seg1))

(define seg2 (make-s (make-p 2 5) (make-p -2 -1)))
(print-p (mid-p seg2))

