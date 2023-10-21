#lang racket

(define/contract (count-pairs n edges)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (define v (build-vector n identity))
  (define (dsu-find! x)
    (define r (vector-ref v x))
    (if (= r x)
        x
        (let ([rr (dsu-find! r)])
          (vector-set! v x rr)
          rr)))
  (define (dsu-unite! x y)
    (define rx (dsu-find! x))
    (define ry (dsu-find! y))
    (vector-set! v rx ry))
  (for ([e (in-list edges)])
    (match e
      [(list a b)
       (dsu-unite! a b)]))
  (define h (make-hasheq))
  (for ([i (in-range (vector-length v))])
    (define x (dsu-find! i))
    (hash-update! h x add1 0))
  (define p2
    (for/sum ([x (in-hash-values h)])
      (* x (- n x))))
  (/ p2 2))

(count-pairs 3 '((0 1) (0 2) (1 2)))
(count-pairs 7 '((0 2) (0 5) (2 4) (1 6) (5 4)))
