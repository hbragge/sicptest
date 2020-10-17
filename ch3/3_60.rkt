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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream-until s n)
  (if (> n 0)
      (begin
        (display-line (stream-car s))
        (display-stream-until (stream-cdr s) (- n 1)))))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))
(define ones (cons-stream 1 ones))

(define (integrate-series s)
  (stream-map / s integers))

; derivative of cosine is negative of sine
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

; derivative of sine is cosine
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
                            (mul-series (stream-cdr s1) s2))))

(define circle-series 
  (add-streams (mul-series cosine-series cosine-series) 
               (mul-series sine-series sine-series))) 
  
; if you see one 1 followed by 0's, your mul-series is correct. 
(display-stream-until circle-series 30)