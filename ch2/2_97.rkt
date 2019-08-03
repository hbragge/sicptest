#lang racket
(require racket/trace)

(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- type-tag" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum -- contents" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (negate x) (apply-generic 'negate x))
(define (reduce x y) (apply-generic 'reduce x y))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  (put 'negate '(scheme-number)
       (lambda (x) (- x)))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (x y) (gcd x y)))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (n d)
         (let ((g (gcd n d)))
           (list (/ n g) (/ d g))))))

(define (make-scheme-number n) n)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ-rat? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (zero-rat? x)
    (zero? (numer x)))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) zero-rat?)
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (zero-poly? p)
    (define (iter l)
      (cond ((empty? l) #t)
            ((not (=zero? (coeff (car l)))) #f)
            (else (iter (cdr l)))))
    (iter (term-list p)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms l1 l2)
    (cond ((empty? l1) l2)
          ((empty? l2) l1)
          (else
           (let ((t1 (car l1)) (t2 (car l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (cdr l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms l1 (cdr l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (cdr l1)
                                (cdr l2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- add-poly" (list p1 p2))))

  (define (neg-terms l)
    (if (empty? l)
        '()
        (let ((t (car l)))
          (adjoin-term
           (make-term (order t)
                      (negate (coeff t)))
           (neg-terms (cdr l))))))

  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))

  (define (sub-terms l1 l2)
    (add-terms l1 (neg-terms l2)))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  
  (define (mul-terms l1 l2)
    (if (empty? l1)
        '()
        (add-terms (mul-term-by-all-terms (car l1) l2)
                   (mul-terms (cdr l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty? l)
        '()
        (let ((t2 (car l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (cdr l))))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- mul-poly" (list p1 p2))))

  (define (div-terms l1 l2)
    (if (empty? l1)
        (list '() '())
        (let ((t1 (car l1))
              (t2 (car l2)))
          (if (> (order t2) (order t1))
              (list '() l1)
              (let ((new-t (make-term
                            (- (order t1) (order t2))
                            (div (coeff t1) (coeff t2)))))
                (let ((rest-of-result
                       (div-terms (sub-terms
                                   l1
                                   (mul-terms (list new-t) l2))
                                  l2)))
                  (list (adjoin-term new-t
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- div-poly" (list p1 p2))))
  
  (define (remainder-terms l1 l2)
    (let*  ((t1 (car l1))
            (o1 (order t1))
            (t2 (car l2))
            (o2 (order t2))
            (c (coeff t2))
            (ic (expt c (+ 1 o1 (- o2)))))
      (cadr 
        (div-terms
          (map-coeffs (lambda (c) (* c ic)) l1)
          l2))))

  (define (gcd-terms l1 l2)
    (if (empty? l2)
        l1
        (let* ((gcd-res (gcd-terms l2 (remainder-terms l1 l2)))
               (coeff-list (map cadr gcd-res))
               (coeff-gcd (apply gcd coeff-list)))
          (map
           (lambda (t)
             (make-term (order t)
                        (/ (coeff t) coeff-gcd)))
           gcd-res))))

  (define (map-coeffs fn term-list)
    "fn is a function that will be called on each
    coefficient in term-list, and is expected to
    return a new coefficient." 
    (map
     (lambda (t)
       (make-term (order t) (fn (coeff t))))
     term-list))

  (define (quotient-terms l1 l2)
    (car (div-terms l1 l2)))
  
  (define (reduce-terms l1 l2)
    (let* ((gcd-rat (gcd-terms l1 l2))
           (c (coeff (car gcd-rat)))
           (o1 (max (order (car l1))
                    (order (car l2))))
           (o2 (order (car gcd-rat)))
           (fact (expt c (+ 1 o1 (- o2))))
           (l1f (map-coeffs
                 (lambda (c) (* c fact)) l1))
           (l2f (map-coeffs
                 (lambda (c) (* c fact)) l2))
           (l1res (quotient-terms l1f gcd-rat))
           (l2res (quotient-terms l2f gcd-rat)))
      (list l1res l2res)))
  
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (reduce-terms
                       (term-list p1)
                       (term-list p2))))
          (list (tag (make-poly (variable p1)
                                (car result)))
                (tag (make-poly (variable p2)
                                (cadr result)))))
        (error "Polys not in same var -- reduce-poly" (list p1 p2))))
  
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (reduce-poly p1 p2)))
  (put 'negate '(polynomial) neg-poly)
  (put '=zero? '(polynomial) zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(define res (add rf1 rf2))

(numer res)
(denom res)
