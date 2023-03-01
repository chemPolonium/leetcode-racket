#lang racket

(define/contract (largest-local grid)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (define m (list->vector (map list->vector grid)))
  (define n (vector-length m))
  (for/list ([i (in-range 1 (sub1 n))])
    (for/list ([j (in-range 1 (sub1 n))])
      (max (vector-ref (vector-ref m (sub1 i)) (sub1 j))
           (vector-ref (vector-ref m (sub1 i)) j)
           (vector-ref (vector-ref m (sub1 i)) (add1 j))
           (vector-ref (vector-ref m i) (sub1 j))
           (vector-ref (vector-ref m i) j)
           (vector-ref (vector-ref m i) (add1 j))
           (vector-ref (vector-ref m (add1 i)) (sub1 j))
           (vector-ref (vector-ref m (add1 i)) j)
           (vector-ref (vector-ref m (add1 i)) (add1 j))))))

(largest-local '((9 9 8 1) (5 6 2 6) (8 2 6 4) (6 2 2 2)))