#lang racket

(define/contract (min-path-cost grid moveCost)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)) exact-integer?)
  (define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define move-cost
    (let ([move-cost-vec (list->vector (map list->vector moveCost))])
      (lambda (a b)
        (vec2d-ref move-cost-vec a b))))
  (for/fold ([prev-row (car grid)]
             [prev-row-cost (car grid)]
             #:result (apply min prev-row-cost))
            ([row (in-list (cdr grid))])
    (define row-cost
      (for/list ([(cell j) (in-indexed (in-list row))])
        (for/fold ([cell-cost 114514])
                  ([prev-cell (in-list prev-row)]
                   [prev-cell-cost (in-list prev-row-cost)])
          (min (+ prev-cell-cost (move-cost prev-cell j) cell) cell-cost))))
    (values row row-cost)))

(min-path-cost '((5 3) (4 0) (2 1)) '((9 8) (1 5) (10 12) (18 6) (2 4) (14 3)))
(min-path-cost '((5 1 2) (4 0 3)) '((12 10 15) (20 23 8) (21 7 1) (8 1 13) (9 10 25) (5 3 2)))
