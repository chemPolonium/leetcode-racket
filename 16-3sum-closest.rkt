#lang racket

(define/contract (three-sum-closest nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v (list->vector (sort nums <)))
  (define l (vector-length v))
  (define closest (+ (vector-ref v 0)
                     (vector-ref v 1)
                     (vector-ref v 2)))
  (for ([i (in-range (- l 2))])
    (define vi (vector-ref v i))
    (let iter ([j (add1 i)] [k (- l 1)])
      (unless (>= j k)
        (define vj (vector-ref v j))
        (define vk (vector-ref v k))
        (define svijk (+ vi vj vk))
        (when (< (abs (- svijk target)) (abs (- closest target)))
          (set! closest svijk))
        (cond [(> svijk target) (iter j (sub1 k))]
              [(< svijk target) (iter (add1 j) k)]
              [else 0]))))
  closest)