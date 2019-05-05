#lang sicp

(define (reverse items)
  (define (iter i result)
    (if (null? i)
        result
        (iter (cdr i) (cons (car i) result))))
  (iter items nil))

(reverse (list 1 4 9 16 25))

(define (deep-reverse items)
  (define (iter i result)
    (cond ((null? i) result)
          ((not (pair? (car i)))
           (iter (cdr i) (cons (car i) result)))
          (else
           (iter (cdr i) (cons (deep-reverse (car i)) result)))))
  (iter items '()))

(deep-reverse (list (list 1 2) (list 3 4)))