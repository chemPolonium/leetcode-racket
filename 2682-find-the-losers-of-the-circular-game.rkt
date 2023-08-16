#lang racket

(define/contract (circular-game-losers n k)
  (-> exact-integer? exact-integer? (listof exact-integer?))
  (let iter ([prev 0] [i 1] [s (seteq 0)])
    (define now (remainder (+ prev (* i k)) n))
    (cond [(set-member? s now)
           (sort (map add1 (set->list (set-subtract (list->seteq (range n)) s))) <)]
          [else (iter now (add1 i) (set-add s now))])))
