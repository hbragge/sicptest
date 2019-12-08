#lang sicp

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-filter pred s)
  (cond ((null? s) '())
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (divisible? x n)
  (= (remainder x n) 0))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) (stream-cdr s))))

(define s1 (partial-sums integers))
(stream-ref s1 0) ; 1
(stream-ref s1 1) ; 3 (1+2)
(stream-ref s1 2) ; 6 (3+3)
(stream-ref s1 3) ; 10 (6+4)