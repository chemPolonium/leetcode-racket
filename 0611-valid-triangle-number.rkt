#lang racket

(define/contract (triangle-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector (sort (filter positive? nums) <)))
  (define n (vector-length v))
  (if (< n 3)
      0
      (for*/sum ([i (in-range (- n 2))]
                 [k (in-range (+ i 2) n)])
        (define j (let bisec ([lo i] [hi k])
                    (define mi (quotient (+ lo hi 1) 2))
                    (cond [(= hi mi) hi]
                          [(> (vector-ref v mi) (- (vector-ref v k) (vector-ref v i)))
                           (bisec lo mi)]
                          [else (bisec mi hi)])))
        (- k j))))

(triangle-number '(2 2 3 4))
