#lang racket
(require racket/trace)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- choose-branch" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)

(define (encode-symbol symbol tree)
  (define (encode-1 current-tree)
    (cond ((leaf? current-tree) '())
          ((memq symbol (symbols (left-branch current-tree))) (cons 0 (encode-1 (left-branch current-tree))))
          ((memq symbol (symbols (right-branch current-tree))) (cons 1 (encode-1 (right-branch current-tree))))
          (else (error "bad symbol -- encode-symbol" symbol))))
  (encode-1 tree))
        
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;(encode '(A D A B B C A) sample-tree)
;(encode (decode sample-message sample-tree) sample-tree)
;sample-message

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge
         (adjoin-set (make-code-tree first second)
                     rest)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define song-tree (generate-huffman-tree '((na 16) (yip 9) (sha 3) (a 2) (get 2) (job 2) (boom 1) (wah 1))))
(define song '(get a job sha na na na na na na na na na wah yip yip sha boom))
(define encoded-song (encode song song-tree))
(decode encoded-song song-tree)

(* 3 (length song)) ; length without huffman
(length encoded-song)