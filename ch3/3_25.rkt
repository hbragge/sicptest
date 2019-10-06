#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter ks records)
        (let ((record (assoc (car ks) records)))
          (if record
              (let ((rest-keys (cdr ks))
                    (record-value (cdr record)))
                (if (null? rest-keys)
                    record-value
                    (iter rest-keys record-value)))
              false)))
      (iter keys (cdr local-table)))
    (define (insert! keys value)
      (define (iter ks records)
        (cond
          ((null? ks) (set-cdr! records value))
          ((or (null? (cdr records)) (not (pair? (cdr records))))
           (set-cdr! records (list (cons (car ks) nil)))
           (iter (cdr ks) (cadr records)))
          (else
           (let ((record (assoc (car ks) (cdr records))))
             (if record
                 (iter (cdr ks) record)
                 (begin (set-cdr! records
                                  (cons (list (car ks))
                                        (cdr records)))
                        (iter (cdr ks) (cadr records))))))))
      (iter keys local-table))
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

(put '(char a) 1)
(put '(char b) 2)
(put '(employee ibm bob) 25)
(put '(employee ibm john) 26)
(get '(char a))
(get '(char b))
(get '(employee))
