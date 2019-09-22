#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; move two pointers with different speed,
; eventually they will collide if there is a cycle
(define (has-cycle? p)
  (define (helper x fwd1 fwd2)
    (cond ((or (null? (cdr x))
               (null? fwd1)
               (null? fwd2)
               (null? (cdr fwd2))
               (null? (cdr (cdr fwd2)))) false)
          ((eq? (car (cdr fwd1)) (car (cdr (cdr fwd2))))
           (display "\n\n")
           (display (car (cdr fwd1)))
           (display "\n")
           (display (car (cdr (cdr fwd2))))
           true)
          (else
           (display "\n\n")
           (display (car (cdr fwd1)))
           (display "\n")
           (display (car (cdr (cdr fwd2))))
           (helper (cdr x) (cdr fwd1) (cdr (cdr fwd2))))))
  (helper p p (cdr p)))

(define x '(a b c d))
(has-cycle? x)
(make-cycle x)
(has-cycle? x)