#lang sicp

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))
            (else (error "Unknown message -- mutex" m))))
    the-mutex))

(define (make-serializer id)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (display "acquired id ")
        (display id)
        (display "\n")
        (let ((val (apply p args)))
          (mutex 'release)
          (display "released id ")
          (display id)
          (display "\n")
          val))
      serialized-p)))

(define (make-account balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (let ((balance-serializer (make-serializer id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (withdraw account amount)
  ((account 'withdraw) amount))

(define (deposit account amount)
  ((account 'deposit) amount))

(define (serialized-withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    ((s w) amount)))

(define (serialized-deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (balance account)
  (account 'balance))

(define (serializer account)
  (account 'serializer))

(define (id account)
  (account 'id))

(define (exchange account1 account2)
  (let ((difference (- (balance account1)
                       (balance account2))))
    (withdraw account1 difference)
    (deposit account2 difference)))

(define (serialized-exchange account1 account2)
  (let ((id1 (id account1))
        (id2 (id account2))
        (s1 (serializer account1))
        (s2 (serializer account2)))
    (if (< id1 id2)
        ((s1 (s2 exchange)) account1 account2)
        ((s2 (s1 exchange)) account1 account2))))

(define a1 (make-account 10 0))
(define a2 (make-account 30 1))
(balance a1)
(balance a2)

(serialized-exchange a1 a2)
(balance a1)
(balance a2)