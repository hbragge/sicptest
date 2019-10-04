#lang sicp

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (item-value item) (car item))
(define (item-next item) (cadr item))
(define (item-prev item) (caddr item))
(define (make-item value) (list value nil nil))
(define (set-item-next! item ptr) (set-car! (cdr item) ptr))
(define (set-item-prev! item ptr) (set-car! (cddr item) ptr))

(define (empty-deque? deque)
  (or (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (cons nil nil))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (front-insert-deque! deque value)
  (let ((new-item (make-item value)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-item-next! new-item (front-ptr deque))
           (set-front-ptr! deque new-item)
           deque))))

(define (rear-insert-deque! deque value)
  (let ((new-item (make-item value)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-item-next! (rear-ptr deque) new-item)
           (set-item-prev! new-item (rear-ptr deque))
           (set-rear-ptr! deque new-item)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (item-next (front-ptr deque)))
         (if (not (empty-deque? deque))
             (set-item-prev! (front-ptr deque) nil))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (item-prev (rear-ptr deque)))
         (if (not (empty-deque? deque))
             (set-item-next! (rear-ptr deque) nil))
         deque)))

(define (print-deque deque)
  (define (print-list l)
    (if (null? l)
        'done
        (begin
          (display (item-value l))
          (newline)
          (print-list (item-next l)))))
  (print-list (front-ptr deque)))

(define q1 (make-deque))
(print-deque q1)
(rear-insert-deque! q1 'b)
(print-deque q1)
(rear-insert-deque! q1 'c)
(print-deque q1)
(front-insert-deque! q1 'a)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(rear-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)