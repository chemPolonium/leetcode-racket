#lang racket

(define/contract (insert intervals newInterval)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof (listof exact-integer?)))
  (cond [(or (null? intervals) (< (second newInterval) (first (first intervals))))
         (cons newInterval intervals)]
        [(< (second (first intervals)) (first newInterval))
         (cons (first intervals) (insert (rest intervals) newInterval))]
        [else
         (insert (rest intervals) (list (min (first (first intervals)) (first newInterval))
                                        (max (second (first intervals)) (second newInterval))))]))
