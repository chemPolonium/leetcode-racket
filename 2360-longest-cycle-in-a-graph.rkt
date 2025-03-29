#lang racket

(define/contract (longest-cycle edges)
  (-> (listof exact-integer?) exact-integer?)
  (define edges-vec (list->vector edges))
  (define visited (make-vector (vector-length edges-vec) -1))
  (define steps (make-vector (vector-length edges-vec)))
  (for/fold ([len -1])
            ([s (in-range (vector-length edges-vec))]
             [c (in-range (vector-length edges-vec))])
    (max len
         (let iter ([i s] [len 0])
           (cond [(= -1 (vector-ref edges-vec i)) -1]
                 [(= c (vector-ref visited i)) (- len (vector-ref steps i))]
                 [(= -1 (vector-ref visited i))
                  (vector-set! visited i c)
                  (vector-set! steps i len)
                  (iter (vector-ref edges-vec i) (add1 len))]
                 [else -1])))))

(longest-cycle '(3 3 4 2 3))
(longest-cycle '(2 -1 3 1))
(longest-cycle '(3 4 0 2 -1 2))
