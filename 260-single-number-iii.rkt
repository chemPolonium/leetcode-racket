#lang racket

(define/contract (single-number nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define ab-xor (apply bitwise-xor nums))
  (define lsb (bitwise-and ab-xor (- ab-xor)))
  (for/fold ([a 0] [b 0] #:result (list a b))
            ([n (in-list nums)])
    (displayln a)
    (displayln b)
    (if (zero? (bitwise-and lsb n))
        (values (bitwise-xor a n) b)
        (values a (bitwise-xor b n)))))

(single-number '(1 2 1 3 2 5))
