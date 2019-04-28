#lang racket

(define (same-parity first . rest)
  (define (equal-parity? a b)
    (= (remainder a 2) (remainder b 2)))
  (define (search-parity a r)
    (if (empty? r)
        r
        (let ((next (car r)))
          (if (equal-parity? a next)
              (cons next (search-parity a (cdr r)))
              (search-parity a (cdr r))))))
  (cons first (search-parity first rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity '())