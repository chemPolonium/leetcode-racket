#lang racket

(require data/heap)

(define/contract (get-number-of-backlog-orders orders)
  (-> (listof (listof exact-integer?)) exact-integer?)

  (define (price<=? order1 order2)
    (<= (first order1) (first order2)))
  (define (price>=? order1 order2)
    (>= (first order1) (first order2)))
  (define (buy? type) (zero? type))
  (define buy (make-heap price>=?))
  (define sell (make-heap price<=?))

  (define (consume-buy buy-price buy-amount)
    (if (zero? (heap-count sell))
        (heap-add! buy (list buy-price buy-amount))
        (match (heap-min sell)
          [(list sell-price sell-amount)
           (cond [(> sell-price buy-price)
                  (heap-add! buy (list buy-price buy-amount))]
                 [(> sell-amount buy-amount)
                  (heap-remove-min! sell)
                  (heap-add! sell (list sell-price (- sell-amount buy-amount)))]
                 [(= sell-amount buy-amount)
                  (heap-remove-min! sell)]
                 [else
                  (heap-remove-min! sell)
                  (consume-buy buy-price (- buy-amount sell-amount))])])))

  (define (consume-sell sell-price sell-amount)
    (if (zero? (heap-count buy))
        (heap-add! sell (list sell-price sell-amount))
        (match (heap-min buy)
          [(list buy-price buy-amount)
           (cond [(< buy-price sell-price)
                  (heap-add! sell (list sell-price sell-amount))]
                 [(> buy-amount sell-amount)
                  (heap-remove-min! buy)
                  (heap-add! buy (list buy-price (- buy-amount sell-amount)))]
                 [(= buy-amount sell-amount)
                  (heap-remove-min! buy)]
                 [else
                  (heap-remove-min! buy)
                  (consume-sell sell-price (- sell-amount buy-amount))])])))

  (for ([order (in-list orders)])
    (match order [(list price amount type)
                  (if (buy? type)
                      (consume-buy price amount)
                      (consume-sell price amount))]))

  (for/fold ([s 0])
            ([o (sequence-append (in-heap/consume! buy) (in-heap/consume! sell))])
    (remainder (+ s (second o)) 1000000007)))

(get-number-of-backlog-orders '[[7 1000000000 1] [15 3 0] [5 999999995 0] [5 1 1]])

(get-number-of-backlog-orders '[[26 7 0] [16 1 1] [14 20 0] [23 15 1] [24 26 0] [19 4 1] [1 1 0]])