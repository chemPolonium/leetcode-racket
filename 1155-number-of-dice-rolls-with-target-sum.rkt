#lang racket

(define/contract (num-rolls-to-target n k target)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (define (f v i j)
    (if (< j i)
        0
        (vector-ref v j)))
  (define v1 (make-vector (add1 target)))
  (for ([i (in-inclusive-range 1 (min k target))])
    (vector-set! v1 i 1))
  (define v2 (make-vector (add1 target)))
  (for ([i (in-inclusive-range 2 n)])
    (for ([j (in-inclusive-range i target)])
      (vector-set! v2 j (+ (f v2 i (sub1 j))
                           (f v1 (sub1 i) (sub1 j))
                           (- (f v1 (sub1 i) (- j k 1))))))
    (vector-copy! v1 0 v2))
  (remainder (vector-ref v1 target) 1000000007))

(num-rolls-to-target 1 6 3)
(num-rolls-to-target 2 6 7)
(num-rolls-to-target 30 30 500)
