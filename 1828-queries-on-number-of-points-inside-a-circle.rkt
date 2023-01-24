#lang racket

(define/contract (count-points points queries)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof exact-integer?))
  (for/list ([q (in-list queries)])
    (match-define (list x y r) q)
    (count (lambda (p) (match-let ([(list px py) p])
                         (<= (sqrt (+ (sqr (- x px)) (sqr (- y py)))) r))) points)))