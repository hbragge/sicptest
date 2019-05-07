#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves0 t)
  (cond ((null? t) 0)
        ((pair? t)
         (+ (count-leaves0 (car t))
            (count-leaves0 (cdr t))))
        (else 1)))

(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (i)
          (if (pair? i)
              (count-leaves i)
              1))
        t)))

(define x (cons (list 1 2) (list 3 4)))
(define x2 (list x x))

(count-leaves0 x2)
(count-leaves x2)