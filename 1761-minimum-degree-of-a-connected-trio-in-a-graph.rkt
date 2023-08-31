#lang racket

(define/contract (min-trio-degree n edges)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (define g1 (make-vector n (seteq)))
  (define g2 (make-vector n (seteq)))
  (define d (make-vector n))
  (for ([e (in-list edges)])
    (match-define (list a b) e)
    (define a1 (sub1 (min a b)))
    (define b1 (sub1 (max a b)))
    (vector-set! d a1 (add1 (vector-ref d a1)))
    (vector-set! d b1 (add1 (vector-ref d b1)))
    (vector-set! g1 a1 (set-add (vector-ref g1 a1) b1))
    (vector-set! g2 b1 (set-add (vector-ref g2 b1) a1)))
  (for*/fold ([m 114514] #:result (if (= m 114514) -1 m))
             ([i (in-range n)]
              [j (in-set (vector-ref g1 i))]
              [k (in-set (vector-ref g1 j))]
              #:when (set-member? (vector-ref g2 k) i))
    (min m (+ (vector-ref d i) (vector-ref d j) (vector-ref d k) -6))))

; (min-trio-degree 6 '((1 2) (1 3) (3 2) (4 1) (5 2) (3 6)))

(min-trio-degree 7 '((1 3) (4 1) (4 3) (2 5) (5 6) (6 7) (7 5) (2 6)))
