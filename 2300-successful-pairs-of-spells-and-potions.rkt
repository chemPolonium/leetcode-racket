#lang racket

(define/contract (successful-pairs spells potions success)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define sorted-potions (list->vector (sort potions >)))
  (define (spell-success spell)
    (define (search l r)
      (if (= l r)
          l
          (let* ([m (quotient (+ l r) 2)]
                 [ms (* spell (vector-ref sorted-potions m))])
            (if (< ms success)
                (search l m)
                (search (add1 m) r)))))
    (search 0 (vector-length sorted-potions)))
  (map spell-success spells))

(successful-pairs '(5 1 3) '(1 2 3 4 5) 7)
