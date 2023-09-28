#lang racket

(require data/heap)

(define/contract (full-bloom-flowers flowers people)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?))
  (define n (length people))
  (define h (make-heap (λ (a b) (<= (second a) (second b)))))
  (define sorted-flowers (sort flowers < #:key first))
  (define sorted-people-pair (sort (map cons people (range n)) < #:key car))
  (define l
    (let iter1 ([flowers sorted-flowers] [people sorted-people-pair] [a null])
      (cond [(empty? people) a]
            [else
             (define p (first people))
             (define n-flowers
               (do ([fs flowers (cdr fs)])
                 ((or (null? fs) (> (caar fs) (car p)))
                  fs)
                 (heap-add! h (car fs))))
             (do () ((or (zero? (heap-count h))
                         (>= (cadr (heap-min h)) (car p))))
               (heap-remove-min! h))
             (iter1 n-flowers (cdr people) (cons (cons (heap-count h) (cdr p)) a))])))
  (define v (make-vector n))
  (for ([p (in-list l)])
    (vector-set! v (cdr p) (car p)))
  (vector->list v))

(full-bloom-flowers '((1 6) (3 7) (9 12) (4 13)) '(2 3 7 11))
