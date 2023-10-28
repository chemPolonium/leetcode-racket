#lang racket

(require data/heap)

(define/contract (pick-gifts gifts k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define h (make-heap >=))
  (heap-add-all! h gifts)
  (for ([_ (in-range k)])
    (define n (integer-sqrt (heap-min h)))
    (heap-remove-min! h)
    (heap-add! h n))
  (for/sum ([n (in-vector (heap->vector h))])
    n))
