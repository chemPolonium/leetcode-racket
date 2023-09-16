#lang racket

(define/contract (queens-attackthe-king queens king)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof (listof exact-integer?)))
  (define queens-set (list->set queens))
  (define (search dx dy)
    (let iter ([x (first king)] [y (second king)])
      (define new-x (+ x dx))
      (define new-y (+ y dy))
      (define new-xy (list new-x new-y))
      (cond [(set-member? queens-set new-xy) new-xy]
            [(or (> new-x 7) (> new-y 7) (< new-x 0) (< new-y 0)) #f]
            [else (iter new-x new-y)])))
  (filter-map (lambda (dx dy) (search dx dy))
              '(1 1 0 -1 -1 -1 0 1)
              '(0 1 1 1 0 -1 -1 -1)))

(queens-attackthe-king '((0 1) (1 0) (4 0) (0 4) (3 3) (2 4)) '(0 0))
