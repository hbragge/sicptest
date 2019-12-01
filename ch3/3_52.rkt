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

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  (display-line sum)
  sum)

(display "def seq\n")
(define seq (stream-map accum (stream-enumi 1 20)))
(display "\ndef y\n")
(define y (stream-filter even? seq))
(display "\ndef z\n")
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(display "\nsum start\n")
(display-line sum)
(display "\ns-ref\n")
(stream-ref y 7)
(display "\nsum\n")
(display-line sum)
(display "\ndisp z\n")
(display-stream z)
(display "\nsum end\n")
(display-line sum)