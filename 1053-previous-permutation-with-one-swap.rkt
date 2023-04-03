#lang racket

(define/contract (prev-perm-opt1 arr)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define arr-vec (list->vector arr))
  (define n (vector-length arr-vec))
  (for ([i (in-inclusive-range (- n 2) 0 -1)]
        #:final (> (vector-ref arr-vec i) (vector-ref arr-vec (add1 i))))
    (when (> (vector-ref arr-vec i) (vector-ref arr-vec (add1 i)))
      (do ([j (sub1 n) (sub1 j)])
        ((and (< (vector-ref arr-vec j) (vector-ref arr-vec i))
              (not (= (vector-ref arr-vec j) (vector-ref arr-vec (sub1 j)))))
         (let ([vi (vector-ref arr-vec i)] [vj (vector-ref arr-vec j)])
           (vector-set! arr-vec i vj)
           (vector-set! arr-vec j vi))))))
  (vector->list arr-vec))

(prev-perm-opt1 '(3 2 1))