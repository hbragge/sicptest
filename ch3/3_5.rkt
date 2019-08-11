#lang racket
(require racket/trace)

(define rand
  (let ((x 0))
    (begin (random-seed 1)
           (lambda ()
             (set! x (random 5000))
             x))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (point-in-region)
    (p (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (monte-carlo trials point-in-region)
     (* (- x2 x1) (- y2 y1))))

(define p1
  (lambda (x y) (<= (+ (expt (- x 5) 2) (expt (- y 7) 2)) 9)))

; circle area = pi*r2 -> pi = area/r2
(/ (estimate-integral p1 2.0 8.0 4.0 10.0 1000) 9.0)

(define unit-circle
  (lambda (x y) (<= (+ (expt x 2) (expt y 2)) 1)))

; integral of unit circle = pi/4
(* 4 (estimate-integral unit-circle 0.0 1.0 0.0 1.0 100000))