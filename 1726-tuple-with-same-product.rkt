#lang racket

(define/contract (tuple-same-product nums)
  (-> (listof exact-integer?) exact-integer?)
  (define nums-vec (list->vector nums))
  (define n (vector-length nums-vec))
  (define h (make-hash))
  (for* ([i (in-range n)]
         [j (in-range (add1 i) n)])
    (define ni (vector-ref nums-vec i))
    (define nj (vector-ref nums-vec j))
    (hash-update! h (* ni nj) add1 0))
  (define a/4
    (for/sum ([p (in-hash-values h)])
      (* p (sub1 p))))
  (* 4 a/4))

(tuple-same-product '(1 2 4 5 10))
