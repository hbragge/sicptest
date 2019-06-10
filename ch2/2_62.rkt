#lang racket
(require racket/trace)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set-orig x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (adjoin-set x set)
  (cond ((element-of-set? x set) set)
        ((null? set) (list x))
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set set1 (cdr set2)))))))))

;(element-of-set? 2 (list 1 2 4))
;(adjoin-set 3 (list 4 5 6))
;(adjoin-set 5 (list 3 4 6))
;(intersection-set (list 1 2 3) (list 2 3 4))
;(trace union-set)
(union-set (list 1 2) (list 3 4))
(union-set (list 2 5) (list 3 4))