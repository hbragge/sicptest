#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval a b)
  (define (iter i)
    (if (> i b)
        nil
        (cons i (iter (+ i 1)))))
  (iter a))

(define (filter cond? seq)
  (flatmap
   (lambda (i)
     (if (cond? i)
         (list i)
         nil))
   seq))

(define empty-board '())

(define (adjoin-position row col rest) 
  (cons (list row col) rest))

(define (safe? positions)
  (let ((trial (car positions))
        (trial-row (caar positions))
        (trial-col (cadar positions))
        (rest (cdr positions)))
    (accumulate (lambda (pos result)
                  (let ((row (car pos))
                        (col (cadr pos)))
                    (and (not (= (- trial-row trial-col)
                                 (- row col)))
                         (not (= (+ trial-row trial-col)
                                 (+ row col)))
                         (not (= trial-row row))
                         result)))
                true
                rest)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 6)