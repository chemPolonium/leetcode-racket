#lang racket

(define/contract (check-powers-of-three n)
  (-> exact-integer? boolean?)
  (let-values ([(q r) (quotient/remainder n 3)])
    (cond [(and (= q 0) (= r 1)) true]
          [(or (= r 1) (= r 0)) (check-powers-of-three q)]
          [else false])))

(check-powers-of-three 12)