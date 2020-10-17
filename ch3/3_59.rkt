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

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))
(define ones (cons-stream 1 ones))

(define (integrate-series s)
  (stream-map / s integers))

; a)
(stream-ref (integrate-series ones) 1)

; b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; derivative of cosine is negative of sine
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

; derivative of sine is cosine
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(stream-ref cosine-series 1)
(stream-ref sine-series 1)