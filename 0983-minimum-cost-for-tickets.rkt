#lang racket

(define/contract (mincost-tickets days costs)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define days-vec (list->vector days))
  (define n (vector-length days-vec))
  (define dp-vec (make-vector n))
  (define (cost-until d)
    (if (< d (car days))
        0
        (let iter ([l 0] [r n])
          (define m (quotient (+ l r) 2))
          (cond [(= m l) (vector-ref dp-vec m)]
                [(> (vector-ref days-vec m) d) (iter l m)]
                [else (iter m r)]))))
  (for ([(d i) (in-indexed (in-list days))])
    (vector-set! dp-vec i
                 (min (+ (first costs) (cost-until (sub1 d)))
                      (+ (second costs) (cost-until (- d 7)))
                      (+ (third costs) (cost-until (- d 30))))))
  (vector-ref dp-vec (sub1 n)))

(mincost-tickets '(2 3 4 6 8 12 14 18 19 26 27 28) '(2 9 31))
