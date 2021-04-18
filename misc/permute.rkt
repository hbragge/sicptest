#lang racket

(define (permute str)
  (define (iter remaining used)
    (if (null? remaining)
        'done
        (begin
          (display (car remaining))
          (iter (cdr remaining) (cons (length remaining) used)))))
  (iter str '()))

(permute '(h e y))
