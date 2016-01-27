#lang racket
(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))

(define (mobile-weight mobile)
 (+
  (branch-weight (left-branch mobile))
  (branch-weight (right-branch mobile))
 )
)

(define (branch-weight branch)
 (if (number? (branch-struct branch))
  (branch-struct branch)
  (mobile-weight (branch-struct branch))
 )
)

(define (mobile-balanced mobile)
 (car (mobile-balance mobile))
)

(define (mobile-balance mobile)
 (let
  (
   [lbalance (branch-balance (left-branch mobile))]
   [rbalance (branch-balance (right-branch mobile))]
  )
  (cons
   (and
    (and (first lbalance) (first rbalance))
    (= (second lbalance) (second rbalance))
   )
   (+ (third lbalance) (third rbalance))
  )
 )
)

(define (branch-balance branch)
 (if (number? (branch-struct branch))
  (list #t (* (branch-struct branch) (branch-length branch)) (branch-struct branch))
  (let ([balance (mobile-balance (branch-struct branch))])
   (list (car balance) (* (cdr balance) (branch-length branch)) (cdr balance))
  )
 )
)

(let
 (
  [one (mk-mobile (mk-branch 7 5) (mk-branch 5 7))]
  [two (mk-mobile (mk-branch 7 5) (mk-branch 5 (mk-mobile (mk-branch 3 4) (mk-branch 4 3))))]
 )
 (and
  (mobile-balanced one)
  (mobile-balanced two)
 )
)

(mobile-balanced (mk-mobile (mk-branch 7 7) (mk-branch 5 (mk-mobile (mk-branch 3 4) (mk-branch 4 3)))))

(require pict)
(require pict/tree-layout)
(define (layout-mobile mobile)
 (tree-layout
  #:pict (cc-superimpose (disk #:color "lightblue" 20) (text (number->string (mobile-weight mobile))))
  (layout-branch (left-branch mobile))
  (layout-branch (right-branch mobile))
 )
)

(define (layout-edges count node)
 (if (= count 0)
  node
  (tree-edge (tree-layout #:pict (blank 1) (layout-edges (- count 1) node)))
 )
)

(define (layout-branch branch)
 (layout-edges (branch-length branch)
  (if (number? (branch-struct branch))
   (tree-layout #:pict (cc-superimpose (disk #:color "green" 20) (text (number->string (branch-struct branch)))))
   (layout-mobile (branch-struct branch))
  )
 )
)

(naive-layered (layout-mobile (mk-mobile (mk-branch 15 7) (mk-branch 5 (mk-mobile (mk-branch 3 4) (mk-branch 4 3))))))
