#lang racket

(define/contract (filter-restaurants restaurants veganFriendly maxPrice maxDistance)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer? exact-integer? (listof exact-integer?))
  (map first
       (sort
        (filter (λ (x)
                  (match x
                    [(list id rating veganFriendly? price distance)
                     (and (or (zero? veganFriendly) (positive-integer? veganFriendly?))
                          (<= price maxPrice)
                          (<= distance maxDistance))]))
                restaurants)
        (λ (a b) (if (= (second a) (second b))
                     (> (first a) (first b))
                     (> (second a) (second b)))))))
