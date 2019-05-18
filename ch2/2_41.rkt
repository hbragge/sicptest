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

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k) (list k j i))
             (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (equal-sum? s)
  (lambda (i)
    (= s (+ (car i) (cadr i) (caddr i)))))

(define (find-triplets-aggregate n s)
  (filter (equal-sum? s) (unique-triples n)))

(find-triplets-aggregate 6 12)





(define (find-triplets-aggregate-orig n s)   
  (define (equal? triplet) 
    (= (cadr triplet) s))
  (define (build-triplet-with-sum i j k) 
    (list (list i j k) (+ i j k)))
  (flatmap (lambda(k) 
             (flatmap (lambda(j) 
                        (filter equal?  
                                (map (lambda(i) (build-triplet-with-sum i j k)) 
                                     (enumerate-interval 1 (- j 1))))) 
                      (enumerate-interval 1 (- k 1)))) 
           (enumerate-interval 1 n)))

(find-triplets-aggregate-orig 6 12)
