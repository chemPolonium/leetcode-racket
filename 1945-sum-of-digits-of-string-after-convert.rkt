#lang racket

(define/contract (get-lucky s k)
  (-> string? exact-integer? exact-integer?)
  (define (sum-digits x)
    (let-values ([(q r) (quotient/remainder x 10)])
      (if (zero? q)
          r
          (+ r (sum-digits q)))))
  (let iter ([k (sub1 k)]
             [res (apply + (map (lambda (c)
                                  (sum-digits (- (char->integer c) 96)))
                                (string->list s)))])
    (if (zero? k)
        res
        (iter (sub1 k) (sum-digits res)))))

(get-lucky "leetcode" 2)