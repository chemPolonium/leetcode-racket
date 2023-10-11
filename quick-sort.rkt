#lang racket

(define (vector-qsort! vec
                       [left 0]
                       [right (sub1 (vector-length vec))])
  (when (< left right)
    (define i (partition-index vec left right))
    (vector-qsort! vec left (sub1 i))
    (vector-qsort! vec (add1 i) right)))

(define (partition-index vec left right)
  ; left as pivot
  (define pivot (vector-ref vec left))
  (let iter ([i (add1 left)] [index (add1 left)])
    (if (<= i right)
        (if (< (vector-ref vec i) pivot)
            (let ([temp (vector-ref vec i)])
              (vector-set! vec i (vector-ref vec index))
              (vector-set! vec index temp)
              (iter (add1 i) (add1 index)))
            (iter (add1 i) index))
        (let ([i1 (sub1 index)])
          (vector-set! vec left (vector-ref vec i1))
          (vector-set! vec (sub1 index) pivot)
          i1))))

(define v (vector 22 30 30 17 33 40 17 23 22 12 20))
(partition-index v 0 (sub1 (vector-length v)))
; (vector-qsort! v)
v
