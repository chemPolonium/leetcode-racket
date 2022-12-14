#lang racket

(define/contract (distance-limited-paths-exist n edgeList queries)
  (-> exact-integer? (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof boolean?))
  (set! edgeList (list->vector edgeList))
  (vector-sort! edgeList < #:key third)
  (define ll (vector-length edgeList))
  (define fa (apply vector (range n)))

  (define (find x)
    (unless (= x (vector-ref fa x))
      (vector-set! fa x (find (vector-ref fa x))))
    (vector-ref fa x))
  (define (merge from to)
    (vector-set! fa (find from) (find to)))

  (define ql (length queries))
  (define a (make-vector ql false))
  (for ([l (in-vector (vector-sort (list->vector (map cons (range ql) queries)) < #:key fourth))])
    (match l [(list i p q limit)
              (for ([k (in-range ll)]
                    #:break (>= (third (vector-ref edgeList k)) limit))
                (merge (first (vector-ref edgeList k))
                       (second (vector-ref edgeList k))))
              (vector-set! a i (= (find p) (find q)))]))
  (vector->list a))

(distance-limited-paths-exist 3 '[[0 1 2] [1 2 4] [2 0 8] [1 0 16]] '[[0 1 2] [0 2 5]])

(distance-limited-paths-exist 5 '[[0 1 10] [1 2 5] [2 3 9] [3 4 13]] '[[0 4 14] [1 4 13]])