#lang racket

(define/contract (put-marbles weights k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define w0w1
    (sort (for/list ([w0 (in-list weights)]
                     [w1 (sequence-tail (in-list weights) 1)])
            (+ w0 w1)) <))
  (if (empty? w0w1)
      0
      (- (apply + (drop w0w1 (- (length w0w1) k -1)))
         (apply + (take w0w1 (sub1 k))))))