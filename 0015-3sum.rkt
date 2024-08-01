#lang racket

(define/contract (three-sum nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (define v (list->vector (sort nums <)))
  (define l (vector-length v))
  (define a (mutable-set))
  (for ([i (in-range (- l 2))])
    (define vi (vector-ref v i))
    (define nvi (- vi))
    (let iter ([j (add1 i)] [k (- l 1)])
      (unless (>= j k)
        (define vj (vector-ref v j))
        (define vk (vector-ref v k))
        (define svjk (+ vj vk))
        (cond [(> svjk nvi) (iter j (sub1 k))]
              [(< svjk nvi) (iter (add1 j) k)]
              [else
               (set-add! a (list vi vj vk))
               (iter (add1 j) (sub1 k))]))))
  (set->list a))