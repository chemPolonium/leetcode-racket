#lang racket

(define/contract (maximum-beauty nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define sorted-nums (sort nums <))
  (let iter ([a sorted-nums] [b sorted-nums] [d 1] [m 0])
    (cond [(null? b) m]
          [(<= (- (car b) (car a)) (* 2 k))
           (iter a (cdr b) (add1 d) (max m d))]
          [else
           (iter (cdr a) b (sub1 d) m)])))

(maximum-beauty '(4 6 1 2) 2)
