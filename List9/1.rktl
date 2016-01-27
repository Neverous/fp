#lang racket
(define count-change
 (let [(memo '())]
  (lambda (amount denominations)
   (let [(mem (assoc (cons amount (length denominations)) memo))]
    (if mem
     (cdr mem)
     (let [(new
            (if (= amount 0)
             1
             (if (empty? denominations)
              0
              (+
               (if (>= amount (first denominations))
                (count-change (- amount (first denominations)) denominations)
                0
               )
               (count-change amount (rest denominations))
              )
             )
            ))]
      (set! memo (cons (cons (cons amount (length denominations)) new) memo))
      new
     )
    )
   )
  )
 )
)

(count-change 1000 '(1 5 10 25 50))
