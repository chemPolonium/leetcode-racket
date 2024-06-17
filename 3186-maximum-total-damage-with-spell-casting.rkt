#lang racket

(define/contract (maximum-total-damage power)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([s0 0] [s1 0] [s2 0] [pp 0] #:result (max s0 s1))
            ([p (in-list (sort power <))])
    (define maxs (max s0 s1))
    (match (- p pp)
      [0 (values (+ s0 p) s1 s2 p)]
      [1 (values (+ s2 p) maxs s1 p)]
      [2 (values (+ (max s1 s2) p) maxs maxs p)]
      [_ (values (+ maxs p) maxs maxs p)])))

(maximum-total-damage '(1 1 3 4))
(maximum-total-damage '(7 1 6 6))
(maximum-total-damage '(5 9 2 10 2 7 10 9 3 8))
(maximum-total-damage '(2 1 4 3 1 1 1 5))
