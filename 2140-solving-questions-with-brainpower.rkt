#lang racket

(define/contract (most-points questions)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define n (length questions))
  (define dp (make-vector (add1 n)))
  (for/fold ([prev-points 0])
            ([current-index (in-naturals)]
             [q (in-list questions)])
    (define points (first q))
    (define brainpower (second q))
    (define current-points (max (vector-ref dp current-index) prev-points))
    (define next-index (min n (+ brainpower current-index 1)))
    (define next-points (max (+ current-points points) (vector-ref dp next-index)))
    (vector-set! dp next-index next-points)
    current-points)
  (vector-ref dp n))

(most-points '((3 2) (4 3) (4 4) (2 5)))
(most-points '((1 1) (2 2) (3 3) (4 4) (5 5)))
