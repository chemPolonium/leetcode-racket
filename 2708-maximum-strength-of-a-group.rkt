#lang racket

(define/contract (max-strength nums)
  (-> (listof exact-integer?) exact-integer?)
  (define-values (pc nc zc)
    (for/fold ([pc 0] [nc 0] [zc 0])
              ([ni (in-list nums)])
      (cond [(zero? ni) (values pc nc (add1 zc))]
            [(positive? ni) (values (add1 pc) nc zc)]
            [else (values pc (add1 nc) zc)])))
  (cond [(= 1 (+ pc nc zc)) (car nums)]
        [(and (zero? pc) (<= nc 1)) 0]
        [else
         (define all-strength (apply * (filter-not zero? nums)))
         (if (positive? all-strength)
             all-strength
             (/ all-strength (apply max (filter negative? nums))))]))
