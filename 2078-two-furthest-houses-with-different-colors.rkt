#lang racket

(define/contract (max-distance colors)
  (-> (listof exact-integer?) exact-integer?)
  (define v (list->vector colors))
  (define n (vector-length v))
  (stream-first
   (stream-filter
    (lambda (i) (and (or (not (= (vector-ref v i) (vector-ref v 0)))
                         (not (= (vector-ref v (- n i 1)) (vector-ref v (sub1 n)))))
                     i))
    (in-range (sub1 n) -1 -1))))

(max-distance '(1 8 3 8 3))
