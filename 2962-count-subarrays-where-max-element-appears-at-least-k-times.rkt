#lang racket

(define/contract (count-subarrays nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define m (apply max nums))
  (define inds (indexes-where nums (lambda (x) (= x m))))
  (if (< (length inds) k)
      0
      (for/sum ([l (in-list inds)]
                [r0 (sequence-tail (in-list inds) (sub1 k))]
                [r1 (sequence-append (sequence-tail (in-list inds) k)
                                     (in-value (length nums)))])
        (* (add1 l) (- r1 r0)))))

(count-subarrays '(1 3 2 3 3) 2)
(count-subarrays '(1 4 2 1) 3)
