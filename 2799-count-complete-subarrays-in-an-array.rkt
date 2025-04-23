#lang racket

(define/contract (count-complete-subarrays nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector nums))
  (define n (vector-length v))
  (define t (set-count (list->seteq nums)))
  (define h (make-hasheq))
  (let iter ([i 0] [j 0] [acc 0])
    (cond [(= (hash-count h) t)
           (hash-update! h (vector-ref v i) sub1)
           (when (zero? (hash-ref h (vector-ref v i)))
             (hash-remove! h (vector-ref v i)))
           (iter (add1 i) j (+ acc (- n j -1)))]
          [(= j n) acc]
          [else
           (hash-update! h (vector-ref v j) add1 0)
           (iter i (add1 j) acc)])))

(count-complete-subarrays '(1 3 1 2 2))
(count-complete-subarrays '(5 5 5 5))
