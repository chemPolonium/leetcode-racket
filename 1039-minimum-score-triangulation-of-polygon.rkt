#lang racket

(define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
(define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
(define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))

(define/contract (min-score-triangulation values)
  (-> (listof exact-integer?) exact-integer?)
  (define values-vec (list->vector values))
  (define l (vector-length values-vec))
  (define dp (make-vec2d l l))
  (for ([d (in-range 2 l)])
    (for ([i (in-naturals)]
          [j (in-range d l)])
      (vec2d-set! dp i j
                  (for/fold ([m 1919810])
                            ([k (in-inclusive-range (add1 i) (sub1 j))])
                    (min m (+ (vec2d-ref dp i k)
                              (vec2d-ref dp k j)
                              (* (vector-ref values-vec i)
                                 (vector-ref values-vec k)
                                 (vector-ref values-vec j))))))))
  (vec2d-ref dp 0 (sub1 l)))

(min-score-triangulation '(1 3 1 4 1 5))