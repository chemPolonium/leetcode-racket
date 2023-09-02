#lang racket

(define/contract (capture-forts forts)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([m 0] [from-empty false] [from-self false] #:result m)
            ([f (in-list (dropf forts zero?))])
    (case f
      [(0) (values m (and from-empty (add1 from-empty)) (and from-self (add1 from-self)))]
      [(1) (if from-empty (values (max m from-empty) false 0) (values m false 0))]
      [(-1) (if from-self (values (max m from-self) 0 false) (values m 0 false))])))

(capture-forts '(1 0 0 -1 0 0 0 0 1))
