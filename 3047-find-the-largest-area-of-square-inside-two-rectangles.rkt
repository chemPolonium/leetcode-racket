#lang racket

(define/contract (largest-square-area bottomLeft topRight)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)) exact-integer?)
  (define bl-vec (list->vector bottomLeft))
  (define tr-vec (list->vector topRight))
  (define n (vector-length bl-vec))
  (for*/fold ([m 0] #:result (sqr m))
             ([i (in-range n)]
              [j (in-range (add1 i) n)])
    (match-define (list b1 l1) (vector-ref bl-vec i))
    (match-define (list b2 l2) (vector-ref bl-vec j))
    (match-define (list t1 r1) (vector-ref tr-vec i))
    (match-define (list t2 r2) (vector-ref tr-vec j))
    (max m (min (- r1 l1) (- r2 l2) (- r2 l1) (- r1 l2)
                (- t1 b1) (- t2 b2) (- t2 b1) (- t1 b2)))))
