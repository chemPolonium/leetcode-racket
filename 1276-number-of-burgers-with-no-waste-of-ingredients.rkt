#lang racket

(define/contract (num-of-burgers tomatoSlices cheeseSlices)
  (-> exact-integer? exact-integer? (listof exact-integer?))
  (define big-mac (/ (- tomatoSlices (* 2 cheeseSlices)) 2))
  (define whopper (- cheeseSlices big-mac))
  (define ans (list big-mac whopper))
  (if (andmap nonnegative-integer? ans)
      ans
      empty))
