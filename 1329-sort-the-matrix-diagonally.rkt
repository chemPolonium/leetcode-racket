#lang racket

; (define/contract (diagonal-sort mat)
;   (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
;   (define mat-vec (list->vector (map list->vector mat)))
;   (define m (vector-length mat-vec))
;   (define n (vector-length (vector-ref mat-vec 0)))
;   (define diagnals-list-vec
;     (for/vector ([k (in-range (+ m n -1))])
;       (for/list ([i (in-range (max 0 (- m k 1)) m)]
;                  [j (in-range (max 0 (- k m -1)) n)])
;         (vector-ref (vector-ref mat-vec i) j))))
;   (vector-map! (lambda (l) (sort l <)) diagnals-list-vec)
;   (define diagnals-sorted-vec-vec
;     (vector-map! list->vector diagnals-list-vec))
;   (for/list ([i (in-range m)])
;     (for/list ([j (in-range n)])
;       (vector-ref (vector-ref diagnals-sorted-vec-vec (+ m (- j i 1)))
;                   (min i j)))))

(define/contract (diagonal-sort mat)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (define mat-vec (list->vector (map list->vector mat)))
  (define m (vector-length mat-vec))
  (define n (vector-length (vector-ref mat-vec 0)))
  (define diagnals-list-vec
    (for/vector ([k (in-range (+ m n -1))])
      (for/list ([i (in-range (max 0 (- m k 1)) m)]
                 [j (in-range (max 0 (- k m -1)) n)])
        (vector-ref (vector-ref mat-vec i) j))))
  (vector-map! (lambda (l) (sort l <)) diagnals-list-vec)
  (for/list ([i (in-range m)])
    (for/list ([j (in-range n)])
      (define k (+ m (- j i 1)))
      (define k-diag (vector-ref diagnals-list-vec k))
      (define x (first k-diag))
      (vector-set! diagnals-list-vec k (rest k-diag))
      x)))

(diagonal-sort '((3 3 1 1) (2 2 1 2) (1 1 1 2)))
