#lang racket

(define/contract (maximum-subsequence-count text pattern)
  (-> string? string? exact-integer?)
  (define p0 (string-ref pattern 0))
  (define p1 (string-ref pattern 1))
  (if (char=? p0 p1)
      ((lambda (x) (/ (* x (add1 x)) 2))
       (count (lambda (c) (char=? c p0)) (string->list text)))
      (for/fold ([c0 0] [c1 0] [acc 0] #:result (+ acc (max c0 c1)))
                ([c (in-string text)])
        (cond [(char=? c p0) (values (add1 c0) c1 acc)]
              [(char=? c p1) (values c0 (add1 c1) (+ acc c0))]
              [else (values c0 c1 acc)]))))
