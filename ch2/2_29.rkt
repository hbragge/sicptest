#lang sicp

(define (make-mobile left right)
  (cons left right))
  ;(list left right))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))
  ;(car (cdr m)))

(define (make-branch length structure)
  (cons length structure))
  ;(list length structure))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))
  ;(car (cdr b)))

(define (total-weight m)
  (cond ((null? m) 0.0)
        ((pair? m) (+ (total-weight (branch-structure (left-branch m)))
                      (total-weight (branch-structure (right-branch m)))))
        (else m)))

(define (torque b)
  (* (branch-length b) (total-weight (branch-structure b))))

(define (balanced? m)
  (if (pair? m)
      (and (= (torque (left-branch m)) (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))
      #t))

(define x0 (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define x (make-mobile (make-branch 2 x0) (make-branch 2 x0)))

(total-weight x0)
(balanced? x0)
(total-weight x)
(balanced? x)