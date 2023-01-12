#lang racket

(define (count-hash l)
  (for/fold ([h (hash)])
            ([i (in-list l)])
    (hash-update h i add1 0)))

(define/contract (rearrange-characters s target)
  (-> string? string? exact-integer?)
  (define target-hash (count-hash (string->list target)))
  (define s-hash (count-hash (filter (lambda (c) (hash-has-key? target-hash c)) (string->list s))))
  (for/fold ([m 114514])
            ([(k v) (in-hash target-hash)])
    (min m (quotient (hash-ref s-hash k 0) v))))

(rearrange-characters "ilovecodingonleetcode" "code")