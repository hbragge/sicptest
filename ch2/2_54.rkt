#lang sicp

; todo: recurse into nested list
(define (equal2? l1 l2)
  (if (and (pair? l1) (pair? l2))
      (and (eq? (car l1) (car l2)) (equal2? (cdr l1) (cdr l2)))
      (eq? l1 l2)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

(equal2? '(this is a list) '(this is a list))
(equal2? '(this is a list) '(this (is a) list))

(equal2? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
;Value: #t 
  
(equal2? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 
;Value: #f

(eq? '(1 2) '(1 2))
; note: identical lists are not eq?