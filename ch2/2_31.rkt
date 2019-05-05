#lang sicp

(define (square x)
  (* x x))

(define (square-tree0 items)
  (cond ((null? items) items)
        ((pair? (car items)) (cons (square-tree0 (car items)) (square-tree0 (cdr items))))
        (else (cons (square (car items)) (square-tree0 (cdr items))))))

(define (square-tree items)
  (cond ((null? items) items)
        ((pair? items) (cons (square-tree (car items)) (square-tree (cdr items))))
        (else (square items))))

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

(square-tree0 x)
(square-tree x)
(square-tree2 x)

(define (tree-map f items)
  (cond ((null? items) items)
        ((pair? (car items)) (cons (tree-map f (car items)) (tree-map f (cdr items))))
        (else (cons (f (car items)) (tree-map f (cdr items))))))

(define (tree-map2 f items)
  (cond ((null? items) items)
        ((pair? items) (cons (tree-map2 f (car items)) (tree-map2 f (cdr items))))
        (else (f items))))

(define (square-tree3 items) (tree-map square items))
(square-tree3 x)

(define (square-tree4 items) (tree-map2 square items))
(square-tree4 x)