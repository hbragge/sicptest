#lang racket
(require racket/trace)

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define t1 (list->tree (list 1 3 5 7 9 11)))
(entry t1)
(left-branch t1)
(right-branch t1)
; 5
; '(1 () (3 () ()))
; '(9 (7 () ()) (11 () ()))

;a. PARTIAL-TREE splits the list ELTS into three parts: the median item THIS-ENTRY,
;the list of items less than the median, and the list of items greater than the median.
;It creates a binary tree whose root node is THIS-ENTRY, whose left subtree is the
;PARTIAL-TREE of the smaller elements, and whose right subtree is the PARTIAL-TREE
;of the larger elements.

;b. t(n) = 2T(n/2)+O(1) = O(n)
