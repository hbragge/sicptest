#lang sicp

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map0 proc s)
  (if (null? s)
      '()
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

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

(define (stream-filter pred s)
  (cond ((null? s) '())
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumi low high)
  (if (> low high)
      '()
      (cons-stream low (stream-enumi (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumi 0 10))) ; only show 0 will be called
(newline)
(stream-ref x 5) ; show 1-5 gets called
(stream-ref x 7) ; show 6-7 gets called