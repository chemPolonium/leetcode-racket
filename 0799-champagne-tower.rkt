#lang racket

(define/contract (champagne-tower poured query_row query_glass)
  (-> exact-integer? exact-integer? exact-integer? flonum?)
  (define (iter r res)
    (if (= r query_row)
        (min 1.0 (list-ref res (min query_glass (- r query_glass))))
        (iter (add1 r) (for/list ([a (sequence-append '(0.0) res)]
                                  [b (sequence-append res '(0.0))])
                         (/ (+ (max 0.0 (sub1 a)) (max 0.0 (sub1 b))) 2)))))
  (iter 0 (list poured)))

(champagne-tower 5 2 2)
(champagne-tower 25 6 1)