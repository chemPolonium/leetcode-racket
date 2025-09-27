#lang racket

(define/contract (largest-perimeter nums)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted (list->vector (sort nums >)))
  (or (for/or ([i (in-range (- (vector-length sorted) 2))])
        (define a (vector-ref sorted i))
        (define b (vector-ref sorted (+ i 1)))
        (define c (vector-ref sorted (+ i 2)))
        (and (< a (+ b c)) (+ a b c)))
      0))
