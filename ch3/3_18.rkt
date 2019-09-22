#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (has-cycle? p)
  (define (helper x visited)
    (cond ((null? (cdr x)) false)
          ((memq (car x) visited)
           (display "\n")
           (display (car x))
           (display "\n")
           (display visited)
           true)
          (else (helper (cdr x) (cons (car x) visited)))))
  (helper p '()))

(define x '(a b c d))
(has-cycle? x)
(make-cycle x)
(has-cycle? x)