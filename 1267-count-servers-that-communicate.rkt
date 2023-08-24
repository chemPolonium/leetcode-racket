#lang racket

(define/contract (count-servers grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define r-sum (map (curry apply +) grid))
  (define c-sum (apply map + grid))
  (for/sum ([r (in-list grid)]
            [r-sum (in-list r-sum)])
    (cond [(zero? r-sum) 0]
          [(> r-sum 1) r-sum]
          [else
           (let iter ([r r] [c-sum c-sum])
             (if (= 1 (car r))
                 (if (= 1 (car c-sum)) 0 1)
                 (iter (cdr r) (cdr c-sum))))])))
