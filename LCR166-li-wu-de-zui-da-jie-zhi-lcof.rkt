#lang racket

(define/contract (max-value grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define grid-vec (list->vector (map list->vector grid)))
  (define m (vector-length grid-vec))
  (define n (vector-length (vector-ref grid-vec 0)))
  (define dp-vec (build-vector m (lambda (_) (make-vector n))))
  (for* ([i (in-range m)]
         [j (in-range n)])
    (vector-set! (vector-ref dp-vec i) j
                 (+ (vector-ref (vector-ref grid-vec i) j)
                    (max (vector-ref (vector-ref dp-vec (max 0 (sub1 i))) j)
                         (vector-ref (vector-ref dp-vec i) (max 0 (sub1 j)))))))
  (vector-ref (vector-ref dp-vec (sub1 m)) (sub1 n)))

(max-value '((1 3 1) (1 5 1) (4 2 1)))