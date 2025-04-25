#lang racket

(define/contract (count-subarrays nums minK maxK)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (for/fold ([ls 0] [hs 0] [s 1] [acc 0] #:result acc)
            ([num (in-list nums)])
    (if (or (< num minK) (> num maxK))
        (values 0 0 1 acc)
        (let ([nls (if (= num minK) s ls)]
              [nhs (if (= num maxK) s hs)])
          (values nls nhs (add1 s) (+ acc (min nls nhs)))))))

(count-subarrays '(1 3 5 2 7 5) 1 5)
(count-subarrays '(1 1 1 1) 1 1)
