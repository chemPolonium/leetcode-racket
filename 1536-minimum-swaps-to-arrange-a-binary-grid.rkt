#lang racket

(define/contract (min-swaps grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define (tail-zeros l)
    (foldl (lambda (li r) (if (zero? li) (add1 r) 0)) 0 l))
  (define v (list->vector (map tail-zeros grid)))
  (define n (vector-length v))
  (define (search-tail i)
    (let iter ([j i])
      (cond [(= j n) false]
            [(>= (vector-ref v j) (- n i 1)) j]
            [else (iter (add1 j))])))
  (define (circular-swap i j)
    (for ([k (in-range j i -1)])
      (define t (vector-ref v k))
      (vector-set! v k (vector-ref v (sub1 k)))
      (vector-set! v (sub1 k) t)))
  (let iter ([i 0] [c 0])
    (cond [(= i n) c]
          [(search-tail i)
           => (lambda (j)
                (circular-swap i j)
                (iter (add1 i) (+ c (- j i))))]
          [else -1])))

(min-swaps '((0 0 1) (1 1 0) (1 0 0)))
(min-swaps '((0 1 1 0) (0 1 1 0) (0 1 1 0) (0 1 1 0)))
