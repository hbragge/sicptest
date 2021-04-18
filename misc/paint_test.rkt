#lang planet neil/sicp

(define mark mark-of-zorro)
;(paint (beside mark (flip-horiz mark)))
(define lower (beside mark (flip-horiz mark)))
(define upper (beside (flip-vert mark) (flip-horiz (flip-vert mark))))

;(paint (below upper lower))













  
(define mark2 (beside mark (flip-vert mark))) 
(define mark4 (below mark2 mark2)) 
  
(define (flipped-pairs painter) 
  (let ((painter2 (beside painter (flip-vert painter)))) 
    (below painter2 painter2))) 
  
;; cannot redefine procedures 
(define mark4a (flipped-pairs mark)) 

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller))))) 
  
;; up-split also here. 
(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (up-split painter (- n 1)))) 
        (below painter (beside smaller smaller))))) 
  
(define (corner-split painter n) 
  (if (= n 0) 
      painter 
      (let ((up (up-split painter (- n 1))) 
            (right (right-split painter (- n 1)))) 
        (let ((top-left (beside up up)) 
              (bottom-right (below right right)) 
              (corner (corner-split painter (- n 1)))) 
          (beside (below painter top-left) 
                  (below bottom-right corner)))))) 
  
(define (square-limit painter n) 
  (let ((quarter (corner-split painter n))) 
    (let ((half (beside (flip-horiz quarter) quarter))) 
      (below (flip-vert half) half))))

;(paint (right-split (below upper lower) 3))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1))))) ; n * (factorial (n - 1))

(factorial 3)
