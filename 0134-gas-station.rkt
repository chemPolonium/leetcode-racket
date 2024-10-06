#lang racket

(define/contract (can-complete-circuit gas cost)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (for/fold ([r 0] [m 0] [s 0] #:result (if (negative? r) -1 s))
            ([(g i) (in-indexed (in-list gas))]
             [c (in-list cost)])
    (define nr (+ r g (- c)))
    (if (< nr m)
        (values nr nr (add1 i))
        (values nr m s))))

(can-complete-circuit '(1 2 3 4 5) '(3 4 5 1 2))
