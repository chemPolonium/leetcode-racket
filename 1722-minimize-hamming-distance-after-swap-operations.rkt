#lang racket

(define/contract (minimum-hamming-distance source target allowedSwaps)
  (-> (listof exact-integer?) (listof exact-integer?) (listof (listof exact-integer?)) exact-integer?)
  (define n (length source))
  (define dsu-fa (build-vector n identity))
  (define dsu-rank (make-vector n))
  (define (dsu-find x)
    (unless (= (vector-ref dsu-fa x) x)
      (vector-set! dsu-fa x (dsu-find (vector-ref dsu-fa x))))
    (vector-ref dsu-fa x))
  (define (dsu-union x y)
    (let ([x (dsu-find x)]
          [y (dsu-find y)])
      (unless (= x y)
        (define xrank (vector-ref dsu-rank x))
        (define yrank (vector-ref dsu-rank y))
        (cond [(< xrank yrank) (vector-set! dsu-fa x y)]
              [(> xrank yrank) (vector-set! dsu-fa y x)]
              [else
               (vector-set! dsu-fa y x)
               (vector-set! dsu-rank x (add1 (vector-ref dsu-rank x)))]))))
  (for ([p (in-list allowedSwaps)])
    (dsu-union (car p) (cadr p)))
  (define sets (make-hash))
  (for ([(si i) (in-indexed source)])
    (define f (dsu-find i))
    (hash-update! (hash-ref! sets f (make-hasheq)) si add1 0))
  (for/fold ([r 0])
            ([(ti i) (in-indexed target)]
             [si (in-list source)])
    (define f (dsu-find i))
    (cond [(positive? (hash-ref (hash-ref sets f) ti 0))
           (hash-update! (hash-ref sets f) ti sub1)
           r]
          [else (add1 r)])))

(minimum-hamming-distance '(1 2 3 4) '(2 1 4 5) '((0 1) (2 3)))
