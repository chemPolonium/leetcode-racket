#lang racket

(define/contract (count-subarrays nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v (list->vector nums))
  (let iter ([l 0] [r 0] [s 0] [acc 0])
    (cond [(>= (* (- r l) s) k) (iter (add1 l) r (- s (vector-ref v l)) acc)]
          [(= r (vector-length v)) (+ acc (- r l))]
          [else (iter l (add1 r) (+ s (vector-ref v r)) (+ acc (- r l)))])))

(count-subarrays '(2 1 4 3 5) 10)
(count-subarrays '(1 1 1) 5)
