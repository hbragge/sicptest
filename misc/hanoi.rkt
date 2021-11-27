#lang racket

(define printmv
  (let ((moves 0))
    (lambda (n from to)
      (set! moves (+ moves 1))
      (display moves)
      (display ": ")
      (display "moving disc ")
      (display n)
      (display " from ")
      (display from)
      (display " to ")
      (displayln to))))

(define (hanoi n from to aux)
  (if (= n 1)
      (printmv n from to)
      (begin
        (hanoi (- n 1) from aux to)
        (printmv n from to)
        (hanoi (- n 1) aux to from))))

(hanoi 5 'a 'b 'c)