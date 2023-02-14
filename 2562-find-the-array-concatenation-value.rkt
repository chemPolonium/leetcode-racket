#lang racket

(define/contract (find-the-array-conc-val nums)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector nums))
  (define (conc x y)
    (string->number (string-append (number->string x) (number->string y))))
  (let iter ([i 0] [j (sub1 (vector-length v))])
    (cond [(< i j) (+ (conc (vector-ref v i) (vector-ref v j)) (iter (add1 i) (sub1 j)))]
          [(= i j) (vector-ref v i)]
          [else 0])))