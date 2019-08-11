#lang racket
(require racket/trace)

(define rand
  (let ((x 0))
    (begin (random-seed 1)
           (lambda (m)
             (cond ((eq? m 'generate)
                    (set! x (random 5000))
                    x)
                   ((eq? m 'reset)
                    (set! x (random-seed 1)))
                   (else (error "Unknown method" m)))))))

(rand 'generate)
(rand 'generate)
(rand 'reset)
(rand 'generate)
