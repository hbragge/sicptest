#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operation expr) 
  (if (memq '+ expr) ; NOTE: precedence defined here: expr is a sum if it contains '+ anywhere
      '+
      '*))
  
(define (sum? expr)
  (eq? '+ (operation expr)))
(define (addend expr)
  (define (iter expr result)
    (if (eq? (car expr) '+)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))
(define (augend expr)
  (let ((result (cdr (memq '+ expr))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? expr) 
  (eq? '* (operation expr)))
(define (multiplier expr) 
  (define (iter expr result) 
    (if (eq? (car expr) '*) 
        result 
        (iter (cdr expr) (append result (list (car expr)))))) 
  (let ((result (iter expr '()))) 
    (if (= (length result) 1) 
        (car result) 
        result)))
(define (multiplicand expr) 
  (let ((result (cdr (memq '* expr)))) 
    (if (= (length result) 1) 
        (car result) 
        result)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (count exp)
  (cond ((number? exp) exp)
        ((product? exp)
         (make-product (count (multiplier exp)) (count (multiplicand exp))))
        ((sum? exp)
         (make-sum (count (addend exp)) (count (augend exp))))
        (else
         (error "unknown expression type -- COUNT" exp))))

(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)
;b
(deriv '(x + 3 * (x + y + 2)) 'x)
(count '(1 + 2 * 3))
; = (make-sum 1 (count '(2 * 3)))
; = (make-sum 1 (make-product 2 3))
; = (make-sum 1 6)
; = 7
(count '(2 * 2 + 3))
; = (make-sum (count '(2 * 2) 3))
; = (make-sum (make-product 2 2) 3)
; = (make-sum 4 3)
; = 7
