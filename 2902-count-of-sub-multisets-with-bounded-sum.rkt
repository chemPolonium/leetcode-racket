#lang racket

(define/contract (count-sub-multisets nums l r)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (define occ (make-hasheq))
  (for ([n (in-list nums)])
    (hash-update! occ n add1 0))
  (define a (make-vector (add1 r)))
  (vector-set! a 0 1)
  (for ([(n c) (in-hash occ)])
    (define v (make-vector (add1 r)))
    (for* ([k (in-inclusive-range 1 c)]
           [i (in-inclusive-range 0 (- r (* k n)))])
      (define s (+ i (* k n)))
      (vector-set! v s (+ (vector-ref v s) (vector-ref a i))))
    (vector-map! + a v))
  (for/fold ([s 0])
            ([i (in-inclusive-range l r)])
    (remainder (+ s (vector-ref a i)) 1000000007)))

(count-sub-multisets '(1 2 2 3) 6 6)
(count-sub-multisets '(2 1 4 2 7) 1 5)
