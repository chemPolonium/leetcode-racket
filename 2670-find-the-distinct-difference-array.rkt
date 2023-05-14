#lang racket

(define/contract (distinct-difference-array nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define v (list->vector nums))
  (define l (vector-length v))
  (define v1 (make-vector l))
  (define v2 (make-vector l))
  (define s1 (mutable-set))
  (define s2 (mutable-set))
  (for ([i (in-range l)])
    (set-add! s1 (vector-ref v i))
    (set-add! s2 (vector-ref v (- l i 1)))
    (vector-set! v1 i (set-count s1))
    (vector-set! v2 (- l i 1) (set-count s2)))
  (for/list ([i (in-vector v1)]
             [j (sequence-append (sequence-tail (in-vector v2) 1) (in-list '(0)))])
    (- i j)))