#lang racket

(define/contract (max-height-of-triangle red blue)
  (-> exact-integer? exact-integer? exact-integer?)
  (max
   (let iter ([r red] [b blue] [i 1])
     (cond [(and (odd? i) (>= r i)) (iter (- r i) b (add1 i))]
           [(and (even? i) (>= b i)) (iter r (- b i) (add1 i))]
           [else (sub1 i)]))
   (let iter ([r blue] [b red] [i 1])
     (cond [(and (odd? i) (>= r i)) (iter (- r i) b (add1 i))]
           [(and (even? i) (>= b i)) (iter r (- b i) (add1 i))]
           [else (sub1 i)]))))
