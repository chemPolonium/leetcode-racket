#lang racket

(define/contract (number-of-arrays differences lower upper)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (for/fold ([lo 0] [hi 0] [c 0] #:result (max 0 (- upper lower -1 (- hi lo))))
            ([d (in-list differences)])
    (define nc (+ c d))
    (values (min nc lo) (max nc hi) nc)))
