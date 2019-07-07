#lang racket
(require racket/trace)

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

(define type-tag car)
(define contents cdr)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method" (list op type-tags))))))

(define (name z) (apply-generic 'name z))
(define (addr z) (apply-generic 'addr z))
(define (salary z) (apply-generic 'salary z))
(define (get-record name file) (apply-generic 'get-record name file))

(define (install-firm1)
  (define (name x)
    (car x))
  (define (addr x)
    (cadar x))
  (define (salary x)
    (caddar x))
  (define (get-record n file)
    (cond ((empty? file) '())
          ((eq? (name (car file)) (car n)) (list 'firm1 (car file)))
          (else (get-record n (cdr file)))))
  (put 'name '(firm1) name)
  (put 'addr '(firm1) addr)
  (put 'salary '(firm1) salary)
  (put 'get-record '(firm1 firm1) get-record))

(define p1_1 '(Jonne Ahokuja 2500))
(define p1_2 '(Kalle Kovakuja 3500))
(define f1 (list 'firm1 p1_1 p1_2))
(install-firm1)

(define (install-firm2)
  (define (name x)
    (car x))
  (define (salary x)
    (cadar x))
  (define (addr x)
    (caddar x))
  (define (do-get-record n file)
    (cond ((empty? file) '())
          ((eq? (name (car file)) (car n)) (list 'firm2 (car file)))
          (else (do-get-record n (cdr file)))))
  (define (get-record n file)
    (do-get-record n (cdr file))) ; strip "metadata"
  (put 'name '(firm2) name)
  (put 'addr '(firm2) addr)
  (put 'salary '(firm2) salary)
  (put 'get-record '(firm2 firm2) get-record))

(define p2_1 '(Sami 4500 Samintie))
(define p2_2 '(Kari 5500 Karikatu))
(define f2 (list 'firm2 'metadata p2_1 p2_2))
(install-firm2)

(addr (get-record '(firm1 Kalle) f1))
(salary (get-record '(firm1 Kalle) f1))

(addr (get-record '(firm2 Sami) f2))
(salary (get-record '(firm2 Sami) f2))

(define (find-record n files)
  (if (empty? files)
      null
      (let ((first (get-record (list (type-tag (car files)) n) (car files))))
        (if (not (empty? first))
            first
            (find-record n (cdr files))))))

(find-record 'Kari (list f1 f2))