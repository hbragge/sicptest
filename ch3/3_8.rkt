#lang racket
(require racket/trace)

(define f
  (let ((count -1))
    (lambda (n)
      (begin (set! count (+ count 1))
             (if (> count 0)
                 0
                 n)))))

;(+ (f 0) (f 1)) ; 0
(+ (f 1) (f 0)) ; 1
