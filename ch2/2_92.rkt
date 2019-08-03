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
;(define (negate x) (apply-generic 'negate x))

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
       (lambda (x) (- x))))

(define (make-scheme-number n) n)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (square x) (* x x))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (equ-rect? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (zero-rect? z1)
    (and (zero? (real-part z1)) (zero? (imag-part z1))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular) equ-rect?)
  (put '=zero? '(rectangular) zero-rect?)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (equ-pol? z1 z2)
    (and (equ? (magnitude z1) (magnitude z2))
         (equ? (angle z1) (angle z2))))
  (define (zero-pol? z1)
    (zero? (magnitude z1)))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar) equ-pol?)
  (put '=zero? '(polar) zero-pol?)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'polar) x y))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; from http://community.schemewiki.org/?sicp-ex-2.92
(define (install-polynomial-package)
  ;; internal procedures 
  ;; representation of poly 
  (define (make-poly variable term-list) 
    (cons variable term-list)) 
  (define (polynomial? p) 
    (eq? 'polynomial (car p))) 
  (define (variable p) (car p)) 
  (define (term-list p) (cdr p)) 
  (define (variable? x) 
    (symbol? x)) 
  (define (same-variable? x y) 
    (and (variable? x) (variable? y) (eq? x y))) 
  
  ;; representation of terms and term lists 
  (define (add-poly p1 p2) 
    ;   (display "var p1 ") (display p1) (newline) 
    ;   (display "var p2 ") (display p2) (newline) 
    (if (same-variable? (variable p1) (variable p2)) 
        (make-poly (variable p1) 
                   (add-terms (term-list p1) 
                              (term-list p2))) 
        (let ((ordered-polys (order-polys p1 p2))) 
          (let ((high-p (higher-order-poly ordered-polys)) 
                (low-p (lower-order-poly ordered-polys))) 
            (let ((raised-p (change-poly-var low-p))) 
              (if (same-variable? (variable high-p)  
                                  (variable (cdr raised-p))) 
                  (add-poly high-p (cdr raised-p)) ;-> cdr for 'polynomial. Should fix this, 
                  ;; change-poly-var should return without 'polynomial as it is only used here. 
                  (error "Poly not in same variable, and can't change either! -- ADD-POLY" 
                         (list high-p (cdr raised-p))))))))) 
  (define (add-terms L1 L2) 
    (cond ((empty-termlist? L1) L2) 
          ((empty-termlist? L2) L1) 
          (else 
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2))) 
             (cond ((> (order t1) (order t2)) 
                    (adjoin-term 
                     t1 (add-terms (rest-terms L1) L2))) 
                   ((< (order t1) (order t2)) 
                    (adjoin-term 
                     t2 (add-terms L1 (rest-terms L2)))) 
                   (else 
                    (adjoin-term 
                     (make-term (order t1) 
                                (add (coeff t1) (coeff t2))) 
                     (add-terms (rest-terms L1) 
                                (rest-terms L2))))))))) 
  
  (define (mul-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2)) 
        (make-poly (variable p1) 
                   (mul-terms (term-list p1) 
                              (term-list p2))) 
        (let ((ordered-polys (order-polys p1 p2))) 
          (let ((high-p (higher-order-poly ordered-polys)) 
                (low-p (lower-order-poly ordered-polys))) 
            (let ((raised-p (change-poly-var low-p))) 
              (if (same-variable? (variable high-p) 
                                  (variable (cdr raised-p))) 
                  (mul-poly high-p (cdr raised-p)) 
                  (error "Poly not in same variable, and can't change either! -- MUL-POLY" 
                         (list high-p (cdr raised-p))))))))) 
  (define (mul-terms L1 L2) 
    (if (empty-termlist? L1) 
        (the-empty-termlist L1) 
        (add-terms (mul-term-by-all-terms (first-term L1) L2) 
                   (mul-terms (rest-terms L1) L2)))) 
  (define (mul-term-by-all-terms t1 L) 
    (if (empty-termlist? L) 
        (the-empty-termlist L) 
        (let ((t2 (first-term L))) 
          (adjoin-term 
           (make-term (+ (order t1) (order t2)) 
                      (mul (coeff t1) (coeff t2))) 
           (mul-term-by-all-terms t1 (rest-terms L)))))) 
  
  (define (div-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2)) 
        (let ((answer (div-terms (term-list p1) 
                                 (term-list p2)))) 
          (list (tag (make-poly (variable p1) (car answer))) 
                (tag (make-poly (variable p1) (cadr answer))))) 
        (let ((ordered-polys (order-polys p1 p2))) 
          (let ((high-p (higher-order-poly ordered-polys)) 
                (low-p (lower-order-poly ordered-polys))) 
            (let ((raised-p (change-poly-var low-p))) 
              (if (same-variable? (variable high-p) 
                                  (variable (cdr raised-p))) 
                  (div-poly high-p (cdr raised-p)) 
                  (error "Poly not in same variable, and can't change either! -- DIV-POLY" 
                         (list high-p (cdr raised-p))))))))) 
  
  (define (div-terms L1 L2) 
    (define (div-help L1 L2 quotient) 
      (if (empty-termlist? L1) 
          (list (the-empty-termlist L1) (the-empty-termlist L1)) 
          (let ((t1 (first-term L1)) 
                (t2 (first-term L2))) 
            (if (> (order t2) (order t1)) 
                (list (cons (type-tag L1) quotient) L1) 
                (let ((new-c (div (coeff t1) (coeff t2))) 
                      (new-o (- (order t1) (order t2)))) 
                  (div-help 
                   (add-terms L1 
                              (mul-term-by-all-terms  
                               (make-term 0 -1) 
                               (mul-term-by-all-terms (make-term new-o new-c) 
                                                      L2))) 
                   L2  
                   (append quotient (list (list new-o new-c))))))))) 
    (div-help L1 L2 '())) 
  
  (define (zero-pad x type) 
    (if (eq? type 'sparse) 
        '() 
        (cond ((= x 0) '())      
              ((> x 0) (cons 0 (zero-pad (- x 1) type))) 
              ((< x 0) (cons 0 (zero-pad (+ x 1) type)))))) 
  ;;; donno what to do when coeff `=zero?` for know just return the  term-list 
  (define (adjoin-term term term-list) 
    (define (adjoin-help term acc term-list) 
      (let ((preped-term ((get 'prep-term (type-tag term-list)) term)) 
            (preped-first-term ((get 'prep-term (type-tag term-list)) 
                                (first-term term-list))) 
            (empty-termlst (the-empty-termlist term-list))) 
        (cond ((=zero? (coeff term)) term-list)  
              ((empty-termlist? term-list) (append empty-termlst 
                                                   acc 
                                                   preped-term 
                                                   (zero-pad (order term) 
                                                             (type-tag term-list)))) 
                
              ((> (order term) (order (first-term term-list))) 
               (append (list (car term-list)) ;-> the type-tag 
                       acc 
                       preped-term  
                       (zero-pad (- (- (order term) 
                                       (order (first-term term-list))) 
                                    1) (type-tag term-list)) 
                       (cdr term-list))) 
              ((= (order term) (order (first-term term-list))) 
               (append (list (car term-list)) 
                       acc 
                       preped-term      ;-> if same order, use the new term 
                       (zero-pad (- (- (order term) 
                                       (order (first-term term-list))) 
                                    1) (type-tag term-list)) 
                       (cddr term-list))) ;-> add ditch the original term. 
              (else 
               (adjoin-help term  
                            (append acc preped-first-term)  
                            (rest-terms term-list)))))) 
    (adjoin-help term '() term-list)) 
  
  (define (negate p) 
    (let ((neg-p ((get 'make-polynomial (type-tag (term-list p))) 
                  (variable p) (list (make-term 0 -1))))) 
      (mul-poly (cdr neg-p) p)))        ; cdr of neg p to eliminat the tag 'polynomial 
  
  (define (zero-poly? p) 
    (define (all-zero? term-list) 
      (cond ((empty-termlist? term-list) #t) 
            (else 
             (and (=zero? (coeff (first-term term-list))) 
                  (all-zero? (rest-terms term-list)))))) 
    (all-zero? (term-list p))) 
  
  (define (equal-poly? p1 p2) 
    (and (same-variable? (variable p1) (variable p2)) 
         (equal? (term-list p1) (term-list p2)))) 
  
  (define (the-empty-termlist term-list) 
    (let ((proc (get 'the-empty-termlist (type-tag term-list)))) 
      (if proc 
          (proc) 
          (error "No proc found -- THE-EMPTY-TERMLIST" term-list)))) 
  (define (rest-terms term-list) 
    (let ((proc (get 'rest-terms (type-tag term-list)))) 
      (if proc 
          (proc term-list) 
          (error "-- REST-TERMS" term-list)))) 
  (define (empty-termlist? term-list) 
    (let ((proc (get 'empty-termlist? (type-tag term-list)))) 
      (if proc 
          (proc term-list) 
          (error "-- EMPTY-TERMLIST?" term-list)))) 
  (define (make-term order coeff) (list order coeff)) 
  (define (order term) 
    (if (pair? term) 
        (car term) 
        (error "Term not pair -- ORDER" term))) 
  (define (coeff term) 
    (if (pair? term) 
        (cadr term) 
        (error "Term not pair -- COEFF" term))) 
  ;; Mixed polynomial operations. This better way to do this, was just to raise the other types 
  ;; to polynomial. Becuase raise works step by step, all coeffs will end up as complex numbers. 
  (define (mixed-add x p)               ; I should only use add-terms to do this.  
    (define (zero-order L)              ; And avoid all this effort. :-S 
      (let ((t1 (first-term L))) 
        (cond ((empty-termlist? L) #f)  
              ((= 0 (order t1)) t1) 
              (else  
               (zero-order (rest-terms L)))))) 
    (let ((tlst (term-list p))) 
      (let ((last-term (zero-order tlst))) 
        (if last-term 
            (make-poly (variable p) (adjoin-term 
                                     (make-term 0 
                                                (add x (coeff last-term))) 
                                     tlst)) 
            (make-poly (variable p) (adjoin-term (make-term 0 x) tlst)))))) 
  
  (define (mixed-mul x p) 
    (make-poly (variable p) 
               (mul-term-by-all-terms (make-term 0 x) 
                                      (term-list p)))) 
  
  (define (mixed-div p x) 
    (define (div-term-by-all-terms t1 L) 
      (if (empty-termlist? L) 
          (the-empty-termlist L) 
          (let ((t2 (first-term L))) 
            (adjoin-term 
             (make-term (- (order t1) (order t2)) 
                        (div (coeff t1) (coeff t2))) 
             (div-term-by-all-terms t1 (rest-terms L)))))) 
    (make-poly (variable p) 
               (div-term-by-all-terms (make-term 0 x) 
                                      (term-list p)))) 
  
  ;; Polynomial transformation. (Operations on polys of different variables) 
  (define (variable-order v)            ;-> var heirarchy tower. x is 1, every other letter 0. 
    (if (eq? v 'x) 1 0)) 
  (define (order-polys p1 p2)           ;-> a pair with the higher order poly `car`, and the 
    (let ((v1 (variable-order (variable p1))) ;-> lower order `cdr` 
          (v2 (variable-order (variable p2)))) 
      (if (> v1 v2) (cons p1 p2) (cons p2 p1)))) 
  (define (higher-order-poly ordered-polys) 
    (if (pair? ordered-polys) (car ordered-polys) 
        (error "ordered-polys not pair -- HIGHER-ORDER-POLY" ordered-polys))) 
  (define (lower-order-poly ordered-polys) 
    (if (pair? ordered-polys) (cdr ordered-polys) 
        (error "ordered-polys not pair -- LOWER-ORDER-POLY" ordered-polys))) 
  
  (define (change-poly-var p)           ;-> All terms must be polys 
    (define (helper-change term-list)   ;-> change each term in term-list 
      (cond ((empty-termlist? term-list) '()) ;-> returns a list of polys with changed var.  
            (else                             ;-> one poly per term.  
             (cons (change-term-var (variable p) 
                                    (type-tag term-list) 
                                    (first-term term-list)) 
                   (helper-change (rest-terms term-list)))))) 
    (define (add-poly-list acc poly-list) ;-> add a list of polys. 
      (if (null? poly-list)               ;-> no more polys, give me the result. 
          acc 
          (add-poly-list (add acc (car poly-list)) ;-> add acc'ed result to first poly 
                         (cdr poly-list)))) ;-> rest of the polys.  
    (add-poly-list 0 (helper-change (term-list p)))) 
  (define (change-term-var original-var original-type term) 
    (make-polynomial original-type (variable (cdr (coeff term))) ;-> cdr eliminates 'polynomial 
                     (map (lambda (x) 
                            (list (order x) ;-> the order in x  
                                  (make-polynomial ;-> coeff is a poly in  
                                   original-type ;-> the original-type (in this example y) 
                                   original-var ;-> the original-var is passed to the coeffs now 
                                   (list        ;-> each term, is formed by  
                                    (list (order term) ;-> the order of the orignal term  
                                          (coeff x)))))) ;-> and the coeff of each term in x 
                          (cdr (term-list (cdr (coeff term))))))) ;-> un-tagged termlist of 
  ;-> the coeff of the term of y. 
  
  ;; interface to rest of the system 
  (define (tag p) (attach-tag 'polynomial p)) 
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2)))) 
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 (negate p2))))) 
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2)))) 
  (put 'negate '(polynomial) 
       (lambda (p) (negate p))) 
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (div-poly p1 p2))) 
  (put 'zero-poly? '(polynomial) 
       (lambda (p) (zero-poly? p))) 
  (put 'equal-poly? '(polynomial polynomial) 
       (lambda (p1 p2) (equal-poly? p1 p2))) 
  (put 'make 'polynomial 
       (lambda (var terms) (tag (make-poly var terms)))) 
    
  ;; Interface of the mixed operations. 
  ;; Addition 
  (put 'add '(scheme-number polynomial) ; because it's commutative I won't define both. Just 
       (lambda (x p) (tag (mixed-add x p)))) ;poly always second. 
  (put 'add '(rational polynomial) 
       (lambda (x p) (tag (mixed-add (cons 'rational x) p)))) ;-> this is needed becuase 
  (put 'add '(real polynomial)                                ;-> apply-generic will remove the 
       (lambda (x p) (tag (mixed-add x p))))                  ;-> tag. 
  (put 'add '(complex polynomial) 
       (lambda (x p) (tag (mixed-add (cons 'complex x) p)))) 
  ;; Subtraction 
  (put 'sub '(scheme-number polynomial) 
       (lambda (x p) (tag (mixed-add x (negate p))))) 
  (put 'sub '(polynomial scheme-number) 
       (lambda (p x) (tag (mixed-add (mul -1 x) p)))) 
  (put 'sub '(rational polynomial) 
       (lambda (x p) (tag (mixed-add (cons 'rational x) (negate p))))) 
  (put 'sub '(polynomial rational) 
       (lambda (p x) (tag (mixed-add (mul -1 (cons 'rational x)) p)))) 
  (put 'sub '(real polynomial) 
       (lambda (x p) (tag (mixed-add x (negate p))))) 
  (put 'sub '(polynomial real) 
       (lambda (p x) (tag (mixed-add (mul -1 x) p)))) 
  (put 'sub '(complex polynomial) 
       (lambda (x p) (tag (mixed-add (cons 'complex x) (negate p))))) 
  (put 'sub '(polynomial complex) 
       (lambda (p x) (tag (mixed-add (mul -1 (cons 'complex x)) p)))) 
  ;; Multiplication 
  (put 'mul '(scheme-number polynomial) 
       (lambda (x p) (tag (mixed-mul x p)))) 
  (put 'mul '(rational polynomial) 
       (lambda (x p) (tag (mixed-mul (cons 'rational x) p)))) 
  (put 'mul '(real polynomial) 
       (lambda (x p) (tag (mixed-mul x p)))) 
  (put 'mul '(complex polynomial) 
       (lambda (x p) (tag (mixed-mul (cons 'complex x) p)))) 
  ;; Division 
  ;; Using a polynomial as a divisor will leave me wiht negative orders. Which I donno how to 
  ;; handle yet. 
  (put 'div '(polynomial scheme-number) 
       (lambda (p x) (tag (mixed-mul (/ 1 x) p)))) 
  (put 'div '(scheme-number polynomial) 
       (lambda (x p) (tag (mixed-div p x)))) 
  (put 'div '(polynomial rational)      ;multiply by the denom, and divide by the numer. 
       (lambda (p x) (tag (mixed-mul (make-rational (cdr x) (car x)) p)))) 
  (put 'div '(rational polynomial) 
       (lambda (x p) (tag (mixed-div p (cons 'rational x))))) 
  (put 'div '(polynomial real)   
       (lambda (p x) (tag (mixed-mul (/ 1.0 x) p)))) 
  (put 'div '(real polynomial) 
       (lambda (x p) (tag (mixed-div p x)))) 
  (put 'div '(polynomial complex) 
       (lambda (p x) (tag (mixed-mul (div 1 (cons 'complex x)) p)))) 
  (put 'div '(complex polynomial) 
       (lambda (x p) (tag (mixed-div p (cons 'complex x))))) 
  'done) 
  
(install-polynomial-package) 
  
; this takes an extra argument type to specify if it is dense or sparse. 
(define (make-polynomial type var terms) 
  (let ((proc (get 'make-polynomial type))) 
    (if proc 
        (proc var terms) 
        (error "Can't make poly of this type -- MAKE-POLYNOMIAL" 
               (list type var terms))))) 
  
; the generic negate procedure needed for subtractions.  
  
(define (negate p) 
  (apply-generic 'negate  p)) 
  
; And the generic first-term procedure with it's package to work with dense and 
; sparse polynomials. 
  
(define (first-term term-list) 
  (let ((proc (get 'first-term (type-tag term-list)))) 
    (if proc 
        (proc term-list) 
        (error "No first-term for this list -- FIRST-TERM" term-list)))) 
  
(define (install-polynomial-term-package) 
  (define (first-term-dense term-list) 
    (if (empty-termlist? term-list) 
        '() 
        (list 
         (- (length (cdr term-list)) 1) 
         (car (cdr term-list)))))   
  (define (first-term-sparse term-list) 
    (if (empty-termlist? term-list) 
        '() 
        (cadr term-list))) 
  (define (prep-term-dense term) 
    (if (null? term) 
        '() 
        (cdr term)))                            ;-> only the coeff for a dense term-list 
  (define (prep-term-sparse term) 
    (if (null? term) 
        '() 
        (list term)))         ;-> (order coeff) for a sparse term-list 
  (define (the-empty-termlist-dense) '(dense)) 
  (define (the-empty-termlist-sparse) '(sparse)) 
  (define (rest-terms term-list) (cons (type-tag term-list) (cddr term-list))) 
  (define (empty-termlist? term-list)  
    (if (pair? term-list)  
        (>= 1 (length term-list)) 
        (error "Term-list not pair -- EMPTY-TERMLIST?" term-list))) 
  (define (make-polynomial-dense var terms) 
    (append (list 'polynomial var 'dense) (map cadr terms))) 
  (define (make-polynomial-sparse var terms) 
    (append (list 'polynomial var 'sparse) terms)) 
  (put 'first-term 'sparse  
       (lambda (term-list) (first-term-sparse term-list))) 
  (put 'first-term 'dense 
       (lambda (term-list) (first-term-dense term-list))) 
  (put 'prep-term 'dense 
       (lambda (term) (prep-term-dense term))) 
  (put 'prep-term 'sparse 
       (lambda (term) (prep-term-sparse term))) 
  (put 'rest-terms 'dense 
       (lambda (term-list) (rest-terms term-list))) 
  (put 'rest-terms 'sparse 
       (lambda (term-list) (rest-terms term-list))) 
  (put 'empty-termlist? 'dense 
       (lambda (term-list) (empty-termlist? term-list))) 
  (put 'empty-termlist? 'sparse 
       (lambda (term-list) (empty-termlist? term-list))) 
  (put 'the-empty-termlist 'dense 
       (lambda () (the-empty-termlist-dense))) 
  (put 'the-empty-termlist 'sparse 
       (lambda () (the-empty-termlist-sparse))) 
  (put 'make-polynomial 'sparse 
       (lambda (var terms) (make-polynomial-sparse var terms))) 
  (put 'make-polynomial 'dense 
       (lambda (var terms) (make-polynomial-dense var terms))) 
  'done) 

(define (make-polynomial-sparse var terms)
  ((get 'make-polynomial 'sparse) var terms))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-term-package)

(define p1 (make-polynomial-sparse 'x (list (list 2 5) (list 0 4)))) ; 5x2 + 4
(define p2 (make-polynomial-sparse 'y (list (list 2 1) (list 0 2))))
; fails
(add p1 p2)
