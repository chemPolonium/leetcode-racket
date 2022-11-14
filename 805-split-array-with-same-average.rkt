#lang racket

(define/contract (split-array-same-average nums)
  (-> (listof exact-integer?) boolean?)
  (define (f return)
    (define n (length nums))
    (define m (quotient n 2))
    (define s (apply + nums))
    (when (for/and ([i (in-range 1 (add1 m))])
            (positive? (remainder (* s i) n)))
      (return false))
    (define dp (make-vector (add1 m) (set)))
    (vector-set! dp 0 (set-add (vector-ref dp 0) 0))
    (for* ([num nums]
           [i (in-range m 0 -1)]
           [x (vector-ref dp (sub1 i))])
      (let ([curr (+ x num)])
        (when (= (* curr n) (* s i))
          (return true))
        (vector-set! dp i (set-add (vector-ref dp i) curr))))
    false)
  (call/cc f))

(split-array-same-average '(1 2 3 4 5 6 7 8))