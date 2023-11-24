#lang racket

(define/contract (count-pairs nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define nums-vec (list->vector (sort nums <)))
  (define n (vector-length nums-vec))
  (let loop ([i 0] [j (sub1 n)] [s 0])
    (cond [(= i j) s]
          [(< (+ (vector-ref nums-vec i) (vector-ref nums-vec j)) target)
           (loop (add1 i) j (+ s (- j i)))]
          [else
           (loop i (sub1 j) s)])))

(count-pairs '(-1 1 2 3 1) 2)
