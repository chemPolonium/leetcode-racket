#lang racket

(define/contract (max-value n index maxSum)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (define (sum-range lo hi)
    (/ (* (+ hi lo) (add1 (- hi lo))) 2))
  (define (s m)
    (+ (sum-range (max 1 (- m index)) (sub1 m))
       m
       (sum-range (max 1 (+ (- m n) index 1)) (sub1 m))))
  (define r (- maxSum n))
  (let iter ([lo 0] [hi maxSum])
    (cond [(= lo hi) lo]
          [else
           (define new-m (quotient (+ hi lo) 2))
           (define new-s (s new-m))
           (cond [(< new-s r) (iter (add1 new-m) hi)]
                 [(= new-s r) (add1 new-m)]
                 [else (iter lo new-m)])])))

(max-value 4 2 6)
(max-value 6 1 10)
(max-value 6 0 10)