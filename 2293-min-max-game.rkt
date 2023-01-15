#lang racket

(define/contract (min-max-game nums)
  (-> (listof exact-integer?) exact-integer?)
  (let iter ([nums nums])
    (if (null? (cdr nums))
        (car nums)
        (iter (for/list ([f (in-cycle (in-value min) (in-value max))]
                         [s (in-slice 2 (in-list nums))])
                (match-define (list a b) s)
                (f a b))))))

(min-max-game '(1 3 5 2 4 8 2 2))