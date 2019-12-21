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

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (partial-stream->list stream n) 
  (define (rec str i) 
    (if (= i n) 
        '() 
        (cons (stream-car str) 
              (rec (stream-cdr str) (+ i 1))))) 
  (rec stream 0)) 

; float representation of (/ num den) with radix as base
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
  
(partial-stream->list (expand 1 7 10) 10) 
;Value: (1 4 2 8 5 7 1 4 2 8) 
  
(/ 1.0 7) 
;Value: .14285714285714285 
  
(partial-stream->list (expand 3 8 10) 5) 
;Value: (3 7 5 0 0) 
  
(/ 3.0 8) 
;Value: .375