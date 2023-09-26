#lang racket

(define/contract (count-ways nums)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted-nums (sort nums <))
  (for/fold ([prev -1] [c 0] #:result c)
            ([(n i) (in-indexed (sequence-append (in-list sorted-nums) (in-value 114514)))])
    (define dc (if (< prev i n) 1 0))
    (values n (+ c dc))))

(count-ways '(1))
(count-ways '(1 1))
(count-ways '(6 0 3 3 6 7 2 7))
