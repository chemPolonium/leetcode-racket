#lang racket

(require racket/format)
(define/contract (discount-prices sentence discount)
  (-> string? exact-integer? string?)
  (define (f s)
    (if (not (regexp-match-exact? #px"\\$\\d+[\\.\\d+]?" s))
        s
        (let* ([price (string->number (substring s 1))]
               [discounted-price (* (- 1 (/ discount 100)) price)]
               [discounted-str (~r discounted-price #:precision '(= 2))])
          (string-append "$" discounted-str))))
  (string-join (map f (string-split sentence))))

(discount-prices "there are $1 $2 and 5$ candies in the shop" 50)
(discount-prices "1 2 $3 4 $5 $6 7 8$ $9 $10$" 100)
