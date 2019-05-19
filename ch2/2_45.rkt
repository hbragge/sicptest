#lang planet neil/sicp

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;(paint (right-split einstein 3))

(define (split op1 op2)
  (define (do-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (do-split painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  do-split)

(define right-split2 (split beside below))
(paint (right-split2 einstein 3))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;(paint (up-split einstein 3))

(define up-split2 (split below beside))
(paint (up-split2 einstein 3))