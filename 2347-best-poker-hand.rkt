#lang racket

(define/contract (best-hand ranks suits)
  (-> (listof exact-integer?) (listof char?) string?)
  (let* ([ranks-hash (foldl (λ (s h) (hash-update h s add1 0)) (hash) ranks)]
         [nums (hash-values ranks-hash)]
         [m (apply max nums)])
    (cond [(apply char=? suits) "Flush"]
          [(>= m 3) "Three of a Kind"]
          [(= m 2) "Pair"]
          [else "High Card"])))