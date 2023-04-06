#lang racket

(define/contract (base-neg2 n)
  (-> exact-integer? string?)
  (if (= n 0)
      "0"
      (list->string
       (let iter ([n n] [neg? false] [l null])
         (define-values (q r) (quotient/remainder n 2))
         (if (= q r 0)
             l
             (iter (if (and neg? (= r 1)) (+ 1 q) q)
                   (not neg?)
                   (cons (integer->char (+ 48 r)) l)))))))

(base-neg2 0)