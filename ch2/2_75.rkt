#lang racket

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown real-imag op" op))))
  dispatch)

(define c1 (make-from-real-imag 3 4))
(c1 'real-part)
(c1 'imag-part)
(c1 'magnitude)
(c1 'angle)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown mag-ang op" op))))
  dispatch)

(define c2 (make-from-mag-ang (c1 'magnitude) (c1 'angle)))
(c2 'real-part)
(c2 'imag-part)
(c2 'magnitude)
(c2 'angle)