#lang racket

;    1
;   1 1
;  1 2 1
; 1 3 3 1

(define (pascal r c)
  (if (or (= c 0) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

(pascal 3 2)
