#lang racket

(define/contract (largest1-bordered-square grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define grid-vec (list->vector (map list->vector grid)))
  (define m (add1 (vector-length grid-vec)))
  (define n (add1 (vector-length (vector-ref grid-vec 0))))
  (define (grid-ref i j) (vector-ref (vector-ref grid-vec i) j))
  (define left (build-vector m (λ (_) (make-vector n))))
  (define up (build-vector m (λ (_) (make-vector n))))
  (define (left-ref i j) (vector-ref (vector-ref left i) j))
  (define (up-ref i j) (vector-ref (vector-ref up i) j))
  (define (left-set! i j v) (vector-set! (vector-ref left i) j v))
  (define (up-set! i j v) (vector-set! (vector-ref up i) j v))
  (define max-border
    (for*/fold ([max-border 0])
               ([i (in-range 1 m)]
                [j (in-range 1 n)]
                #:when (positive? (grid-ref (sub1 i) (sub1 j))))
      (left-set! i j (add1 (left-ref i (sub1 j))))
      (up-set! i j (add1 (up-ref (sub1 i) j)))
      (define border
        (let iter ([border (min (left-ref i j) (up-ref i j))])
          (if (or (< (left-ref (add1 (- i border)) j) border)
                  (< (up-ref i (add1 (- j border))) border))
              (iter (sub1 border))
              border)))
      (max border max-border)))
  (* max-border max-border))