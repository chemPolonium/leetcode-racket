#lang racket

(require data/heap)

(define/contract (max-kelements nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define h (make-heap >=))
  (heap-add-all! h nums)
  (for/sum ([_ (in-range k)])
    (define m (heap-min h))
    (heap-remove-min! h)
    (heap-add! h (ceiling (/ m 3)))
    m))
