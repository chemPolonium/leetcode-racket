#lang racket

(define/contract (find-indices nums indexDifference valueDifference)
  (-> (listof exact-integer?) exact-integer? exact-integer? (listof exact-integer?))
  (define nums-vec (list->vector nums))
  (for*/fold ([a #f] #:result (or a '(-1 -1)))
             ([i (in-range (vector-length nums-vec))]
              [j (in-range (+ i indexDifference) (vector-length nums-vec))]
              #:break a)
    (and (>= (abs (- (vector-ref nums-vec i) (vector-ref nums-vec j))) valueDifference)
         (list i j))))

(find-indices '(5 1 4 1) 2 4)
(find-indices '(2 1) 0 0)
