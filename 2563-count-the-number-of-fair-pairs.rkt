#lang racket

(define (vector-lower-bound vec value [cmp -])
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [comparison (cmp (vector-ref vec i) value)])
          (if (negative? comparison)
              (iter (add1 i) end)
              (iter start i))))))

(define (vector-upper-bound vec value [cmp -])
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        start
        (let* ([i (quotient (+ start end) 2)]
               [comparison (cmp (vector-ref vec i) value)])
          (if (positive? comparison)
              (iter start i)
              (iter (add1 i) end))))))

(define/contract (count-fair-pairs nums lower upper)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (define v (list->vector (sort nums <)))
  (define l (vector-length v))
  (define (numi i) (vector-ref v i))
  (for/sum ([i (in-range l)])
    (define numii (numi i))
    (- (max (add1 i) (vector-upper-bound v (- upper numii)))
       (max (add1 i) (vector-lower-bound v (- lower numii))))))