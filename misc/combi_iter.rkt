#lang racket

(define (nth n items)
  (if (null? items)
      'error
      (if (= n 0)
          (car items)
          (nth (- n 1) (cdr items)))))

(define (append a b)
  (if (null? a)
      (cons b '())
      (cons (car a) (append (cdr a) b))))

(define (combi str outstr level max)
  (define (iter n)
    (if (>= n max)
        'iterdone
        (if (null? str)
            'combidone
            (let ((res (append outstr (nth n str))))
              (display res)
              (newline)
              (combi str res (+ n 1) max)
              (iter (+ n 1))))))
  (iter level))

(define str '(a b c))
(combi str '() 0 (length str))
