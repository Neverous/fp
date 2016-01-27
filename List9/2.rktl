#lang racket
(define (count-atoms lst)
 (count (lambda (x) (not (pair? x))) lst)
)

(count-atoms '(1 2 3 4 #t "6" `(7 8)))
