#lang racket
(require racket/trace)

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator x)
  (list (car x)))

(define (sum? x)
  (eq? (operator x) '+))
(define (addend s) (car s))
(define (augend s)
  (if (> (length s) 2)
      (make-sum (addend (cdr s)) (augend (cdr s)))
      (cadr s)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (eq? (operator x) '*))
(define (multiplier p) (car p))
(define (multiplicand p)
  (if (> (length p) 2)
      (make-product (multiplier (cdr p)) (multiplicand (cdr p)))
      (cadr p)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (eq? (operator) '**))
(define (base p) (car p))
(define (exponent p) (cadr p))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define deriv-sum
  (lambda (exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var))))

(define deriv-prod
  (lambda (exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp)))))

(define deriv-exp
  (lambda (exp var)
    (let ((e (exponent exp))
          (b (base exp)))
      (make-product
       (make-product e
                     (make-exponentiation b (- e 1)))
       (deriv b var)))))

(put 'deriv '(+) deriv-sum)
(put 'deriv '(*) deriv-prod)
(put 'deriv '(**) deriv-exp)

(define operands cdr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

;(trace deriv)
;(trace deriv-sum)
;(trace deriv-prod)
;(trace multiplicand)
(deriv '(** (* x y) 2) 'x)
(deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)
(deriv '(* x y) 'y)
(deriv '(* x y (+ x 3)) 'x)
