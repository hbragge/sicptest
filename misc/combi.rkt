#lang racket

(define (combi str len outstr level)
  (if (= level len)
      'done
      (begin
        (let ((outstr_new (cons (car str) outstr)))
          (display outstr_new)
          (newline)
          (combi (cdr str) len outstr_new (+ level 1)))
        (combi (cdr str) (- len 1) outstr level))))

(define str '(a b c))
(combi str (length str) '() 0)