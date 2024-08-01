#lang racket

(define/contract (distribute-candies candyType)
  (-> (listof exact-integer?) exact-integer?)
  (min (set-count (list->seteq candyType))
       (ceiling (/ (length candyType) 2))))
