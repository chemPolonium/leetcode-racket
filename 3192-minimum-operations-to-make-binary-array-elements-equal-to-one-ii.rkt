#lang racket

(define/contract (min-operations nums)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([fliped false] [a 0] #:result a)
            ([ni (in-list nums)])
    (if (boolean=? (zero? ni) fliped)
        (values fliped a)
        (values (not fliped) (add1 a)))))
