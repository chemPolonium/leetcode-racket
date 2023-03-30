#lang racket

(define/contract (max-width-of-vertical-area points)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let iter ([pts (sort (map first points) <)] [m 0])
    (match pts
      [(list a b r ...)
       (iter (cons b r) (max m (- b a)))]
      [_ m])))

(max-width-of-vertical-area '((8 7) (9 9) (7 4) (9 7)))