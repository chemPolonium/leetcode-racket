#lang racket

(define/contract (median-of-uniqueness-array nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector nums))
  (define n (vector-length v))
  (define m (quotient (add1 (quotient (* n (add1 n)) 2)) 2))
  (define (check t)
    (define cnt (make-hash))
    (for/fold ([j 0] [tot 0] #:result (>= tot m))
              ([(vi i) (in-indexed v)])
      (hash-update! cnt vi add1 0)
      (define new-j
        (do ([j j (add1 j)])
          [(<= (hash-count cnt) t) j]
          (let ([vj (vector-ref v j)])
            (hash-update! cnt vj sub1)
            (when (zero? (hash-ref cnt vj))
              (hash-remove! cnt vj)))))
      (define new-tot (+ tot (add1 (- i new-j))))
      (values new-j new-tot)))
  (let iter ([lo 1] [hi n] [res 0])
    (if (> lo hi)
        res
        (let ([mid (quotient (+ lo hi) 2)])
          (if (check mid)
              (iter lo (sub1 mid) mid)
              (iter (add1 mid) hi res))))))
