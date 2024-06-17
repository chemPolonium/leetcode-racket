#lang racket

(define/contract (count-complete-day-pairs hours)
  (-> (listof exact-integer?) exact-integer?)
  (define v (make-vector 24))
  (for ([h (in-list hours)])
    (define r (remainder h 24))
    (vector-set! v r (add1 (vector-ref v r))))
  (define s1
    (for/sum ([i (in-inclusive-range 1 11)])
      (* (vector-ref v i) (vector-ref v (- 24 i)))))
  (define (cn2 x) (if (zero? x) 0 (/ (* x (sub1 x)) 2)))
  (define s0 (cn2 (vector-ref v 0)))
  (define s2 (cn2 (vector-ref v 12)))
  (+ s0 s1 s2))
