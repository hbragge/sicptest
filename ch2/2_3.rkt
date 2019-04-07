#lang racket

(define (make-p x y)
  (cons x y))

(define (x-co p)
  (car p))

(define (y-co p)
  (cdr p))

(define (make-s start end)
  (cons start end))

(define (start-s seg)
  (car seg))

(define (end-s seg)
  (cdr seg))

(define (sq n) (* n n))

(define (len-s seg)
  (let ((p1 (start-s seg)) 
        (p2 (end-s seg))) 
    (let ((x1 (x-co p1)) 
          (y1 (y-co p1)) 
          (x2 (x-co p2)) 
          (y2 (y-co p2))) 
      (sqrt (+ (sq (- x1 x2)) 
               (sq (- y1 y2)))))))

(define (make-r n e s w)
  (cons n (cons e (cons s (cons w 'empty)))))

(define (r-n r)
  (car r))

(define (r-e r)
  (car (cdr r)))

(define (r-s r)
  (car (cdr (cdr r))))

(define (r-w r)
  (car (cdr (cdr (cdr r)))))

(define (perim r)
  (+ (len-s (r-n r)) (len-s (r-e r)) (len-s (r-s r)) (len-s (r-w r))))

(define (area r)
  (* (len-s (r-n r)) (len-s (r-e r))))

(define n (make-s (make-p 1 1) (make-p 3 1)))
(define e (make-s (make-p 3 1) (make-p 3 -2)))
(define s (make-s (make-p 3 -2) (make-p 1 -2)))
(define w (make-s (make-p 1 -2) (make-p 1 1)))
(define rect1 (make-r n e s w))
(perim rect1)
(area rect1)