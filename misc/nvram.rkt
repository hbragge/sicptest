#lang sicp

(define (print x)
  (display x)
  (display "\n"))

(define (reportDone progress totalLen)
  (display "\ndone reading total ")
  (display progress)
  (display " out of ")
  (display totalLen)
  (display "\n"))

(define (reportProg progress len)
  (display "\nread bytes ")
  (display progress)
  (display "-")
  (print (+ progress (- len 1))))

(define (doReportCopy progress len)
  (display progress)
  (display "-")
  (display (+ progress (- len 1)))
  (print " to user"))

(define (reportCopy progress len)
  (display "copy bytes ")
  (doReportCopy progress len))

(define (reportCopyPart progress len)
  (display "copy partial bytes ")
  (doReportCopy progress len))

(define stepLen 4)
(define blockLen 12)

(define (read offset userLen)
  (define (readImpl idx idxForUser len)
    (if (>= idx blockLen)
        (reportDone idx blockLen)
        (begin
          (reportProg idx len)
          (cond ((and (>= idx offset) (< idx (+ offset userLen)))
                   (let ((localLen (min len (- userLen idxForUser))))
                     (reportCopy idx localLen)
                     (readImpl (+ idx len) (+ idxForUser localLen) stepLen)))
                ((and (< idx offset) (> (+ idx len) offset))
                 (let ((localOffset (- offset idx)))
                   (let ((localLen (min (- len localOffset) (- userLen idxForUser))))
                     (reportCopyPart (+ idx localOffset) localLen)
                     (readImpl (+ idx len) (+ idxForUser localLen) stepLen))))
                (else
                 (readImpl (+ idx len) idxForUser stepLen))))))
  (readImpl 0 0 stepLen))

(read 0 8) ; 0-3, 4-7
;(read 0 12) ; 0-3, 4-7, 8-11
;(read 1 7) ; 1-3, 4-7
;(read 1 8) ; 1-3, 4-7, 8-8
;(read 1 2) ; 1-2
;(read 5 2) ; 5-6
;(read 9 3) ; 9-11
;(read 9 2) ; 9-10
;(read 3 6) ; 3-3, 4-7, 8-8
;(read 7 1) ; 7-7