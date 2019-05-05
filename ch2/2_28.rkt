#lang sicp

(define (fringe items)
  (define (iter i result)
    (cond ((null? i) result)
          ((not (pair? (car i)))
           (iter (cdr i) (append result (list (car i)))))
          (else
           (iter (cdr i) (append result (fringe (car i)))))))
  (iter items '()))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
(fringe '(2))