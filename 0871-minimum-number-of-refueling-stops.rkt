#lang racket

(require data/heap)

(define/contract (min-refuel-stops target startFuel stations)
  (-> exact-integer? exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (define n (length stations))
  (define h (make-heap >))
  (define fuel
    (for/fold ([prev-posi 0] [fuel startFuel] #:result fuel)
              ([station (sequence-append (in-list stations) (in-value (list target 0)))]
               #:break (negative? fuel))
      (match-define (list position-i fuel-i) station)
      (define subed-fuel (- fuel (- position-i prev-posi)))
      (define added-fuel
        (let iter ([unadded-fuel subed-fuel])
          (if (and (positive? (heap-count h)) (negative? unadded-fuel))
              (let ([added-fuel (+ unadded-fuel (heap-min h))])
                (heap-remove-min! h)
                (iter added-fuel))
              unadded-fuel)))
      (heap-add! h fuel-i)
      (values position-i added-fuel)))
  (if (negative? fuel)
      -1
      (- n (heap-count h) -1)))

(min-refuel-stops 100 1 '())
