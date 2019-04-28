#lang racket

(define (last-pair l)
  (if (< (length l) 2)
      l
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34))
(last-pair '())
