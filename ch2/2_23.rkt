#lang sicp

(define (for-each2 f items)
  (cond ((null? items) (newline))
        (else (f (car items))
              (for-each2 f (cdr items)))))

(for-each2 (lambda (x) (newline) (display x))
          (list 57 321 88))

