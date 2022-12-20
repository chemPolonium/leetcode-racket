#lang racket

(define/contract (minimum-size nums maxOperations)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let iter ([l 0] [r (apply max nums)] [ans 0])
    (if (> l r)
        ans
        (let* ([y (quotient (+ l r) 2)]
               [z (if (zero? y) 1 y)]
               [ops (apply + (map (lambda (x) (quotient (sub1 x) z))
                                  nums))])
          (if (> ops maxOperations)
              (iter (add1 y) r ans)
              (iter l (sub1 y) z))))))