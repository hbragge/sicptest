#lang sicp

; accumulate
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (first already-reversed)
                (append already-reversed (list first)))
              nil
              sequence))

(define (reverse2 sequence)
  (fold-left (lambda (result first) (cons first result)) nil sequence))

(reverse (list 1 2 3))
(reverse2 (list 1 2 3))
