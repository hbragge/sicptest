#lang sicp

(define (square x)
  (* x x))

(define (square-tree items)
  (cond ((null? items) items)
        ((pair? (car items)) (cons (square-tree (car items)) (square-tree (cdr items))))
        (else (cons (square (car items)) (square-tree (cdr items))))))

(define (square-tree2 items)
  (map (lambda (i)
         (if (pair? i)
             (square-tree2 i)
             (square i)))
       items))

(define x
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree x)
(square-tree2 x)