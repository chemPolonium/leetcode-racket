#lang racket

(define/contract (min-taps n ranges)
  (-> exact-integer? (listof exact-integer?) exact-integer?)
  (define right-most (build-vector (add1 n) identity))
  (for ([(r i) (in-indexed ranges)])
    (define start (max 0 (- i r)))
    (define end (min n (+ i r)))
    (vector-set! right-most start (max (vector-ref right-most start) end)))
  (let iter ([last 0] [ret 0] [pre 0] [i 0])
    (cond [(= i n) ret]
          [else
           (define new-last (max last (vector-ref right-most i)))
           (cond [(= i new-last) -1]
                 [(= i pre) (iter new-last (add1 ret) new-last (add1 i))]
                 [else (iter new-last ret pre (add1 i))])])))