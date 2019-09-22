#lang racket

(define (count-pairs p)
  (define (element-of? x set)
    (if (null? set)
        false
        (or (eq? x (car set)) (element-of? x (cdr set)))))
  (define (count x visited)
    (if (not (pair? x))
        0
        (let ((visited-new (cons (car x) visited)))
          (+ (count (car x) visited-new)
             (count (cdr x) visited-new)
             (if (element-of? x visited)
                 0
                 1)))))
  (count p '()))

(define x (cons 'a 'b))
(define z1 (cons x x))

(count-pairs z1)