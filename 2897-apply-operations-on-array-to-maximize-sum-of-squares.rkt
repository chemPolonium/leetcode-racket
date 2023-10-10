#lang racket

(define/contract (max-sum nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v (make-vector 30))
  (for ([n (in-list nums)])
    (let iter ([n n] [i 0])
      (define-values (q r) (quotient/remainder n 2))
      (vector-set! v i (+ r (vector-ref v i)))
      (unless (zero? q)
        (iter q (add1 i)))))
  (define s
    (for/sum ([i (in-inclusive-range 1 k)])
      (define n
        (for/fold ([acc 0])
                  ([b (in-inclusive-range 29 0 -1)])
          (+ (* acc 2) (if (<= i (vector-ref v b)) 1 0))))
      (* n n)))
  (remainder s 1000000007))

(max-sum '(2 6 5 8) 2)
