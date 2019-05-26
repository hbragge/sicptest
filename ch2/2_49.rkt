#lang sicp
(#%require 2htdp/image)
(#%require lang/posn)
(#%require racket/base)

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w)) (+ (ycor-vect v) (ycor-vect w))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define *current-image* empty-image)  

(define (*new-image* new-frame)
  (define (xy->posn x y)
    (let ((v ((frame-coord-map new-frame) (make-vect x y))))
      (make-posn (xcor-vect v) (ycor-vect v))))
  (set! *current-image*
        (polygon
         (list
          (xy->posn 0 0)
          (xy->posn 0 1)
          (xy->posn 1 1)
          (xy->posn 1 0))
         "solid"
         "white")))  

(define (draw-line start end)
    (set! *current-image*
        (add-line
         *current-image*
         (xcor-vect start)
         (ycor-vect start)
         (xcor-vect end)
         (ycor-vect end)
         "white")))

(define (paint-in-frame painter frame)
    (*new-image* frame)
    (painter frame)
    *current-image*)  

(define (paint painter)
    (paint-in-frame
        painter
        (make-frame
            (make-vect 0 150)
            (make-vect 150 0)
            (make-vect 0 -150))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define tl (make-vect 0 1))
(define tr (make-vect 1 1))
(define bl (make-vect 0 0))
(define br (make-vect 1 0))

;a
(define segs-outline (list
                      (make-segment bl tl)
                      (make-segment tl tr)
                      (make-segment tr br)
                      (make-segment br bl)))

;b
(define segs-x (list
                (make-segment bl tr)
                (make-segment tl br)))

(define b (make-vect 0.5 0))
(define l (make-vect 0 0.5))
(define t (make-vect 0.5 1))
(define r (make-vect 1 0.5))

;c
(define diamond (list
                 (make-segment b l)
                 (make-segment l t)
                 (make-segment t r)
                 (make-segment r b)))

(define frame1 (make-frame (make-vect 0 0) (make-vect 1 1) (make-vect 2 2)))

((segments->painter segs-outline) frame1)