#lang racket

(define/contract (minimum-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (define nums-vector (list->vector nums))
  (define n (vector-length nums-vector))
  (define lmin (make-vector n))
  (define rmin (make-vector n))
  (for/fold ([m (vector-ref nums-vector 0)])
            ([i (in-inclusive-range 1 (- n 2))])
    (vector-set! lmin i m)
    (min m (vector-ref nums-vector i)))
  (for/fold ([m (vector-ref nums-vector (sub1 n))])
            ([i (in-inclusive-range (- n 2) 1 -1)])
    (vector-set! rmin i m)
    (min m (vector-ref nums-vector i)))
  (define a
    (vector-map (λ (l m r) (if (and (< l m) (< r m)) (+ l m r) -1))
                lmin nums-vector rmin))
  (for/fold ([m 114514114514] #:result (if (= m 114514114514) -1 m))
            ([i (in-inclusive-range 1 (- n 2))])
    (define ai (vector-ref a i))
    (if (= ai -1)
        m
        (min m ai))))

(minimum-sum '(8 6 1 5 3))
