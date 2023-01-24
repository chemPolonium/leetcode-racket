#lang racket

(require racket/flonum)

(define/contract (calculate-tax brackets income)
  (-> (listof (listof exact-integer?)) exact-integer? flonum?)
  (let iter ([brackets brackets] [prev-upper 0])
    (define bracket (first brackets))
    (define upper (first bracket))
    (define percent (second bracket))
    (if (<= income upper)
        (* 0.01 (->fl percent) (->fl (- income prev-upper)))
        (+ (* 0.01 percent (- upper prev-upper))
           (iter (rest brackets) upper)))))

(calculate-tax '[[3 50] [7 10] [12 25]] 10)