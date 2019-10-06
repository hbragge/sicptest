#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((equal? m 'lookup-proc) lookup)
            ((equal? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (same-key-1? a b)
  (equal? a b))

(define op-table (make-table same-key-1?))
(define get (op-table 'lookup-proc))
(define put (op-table 'insert-proc!))

(put 'a 1)
(put 'b 2)
(get 'a)
(get 'b)
(get 'c)
