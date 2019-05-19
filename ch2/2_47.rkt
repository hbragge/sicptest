#lang sicp

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 f)
  (car f))
(define (edge1-frame2 f)
  (cadr f))
(define (edge2-frame2 f)
  (cddr f))

(define f1 (make-frame 0 2 4))
(origin-frame f1)
(edge1-frame f1)
(edge2-frame f1)

(define f2 (make-frame2 0 2 4))
(origin-frame2 f2)
(edge1-frame2 f2)
(edge2-frame2 f2)