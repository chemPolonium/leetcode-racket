#lang racket

(define/contract (minimum-time time totalTrips)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (trips tt)
    (for/sum ([t (in-list time)])
      (quotient tt t)))
  (define (search-up lo hi)
    (if (>= (trips hi) totalTrips)
        (values lo hi)
        (search-up hi (* 2 hi))))
  (define (search-down lo hi)
    (define mid (quotient (+ lo hi 1) 2))
    (cond [(= mid hi) mid]
          [(< (trips mid) totalTrips) (search-down mid hi)]
          [else (search-down lo mid)]))
  (define-values (lo hi) (search-up 0 1))
  (search-down lo hi))

(minimum-time '(1 2 3) 5)
