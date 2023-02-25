#lang racket

(define/contract (minimum-swap s1 s2)
  (-> string? string? exact-integer?)
  (define-values [xy yx]
    (for/fold ([xy 0] [yx 0])
              ([c1 (in-string s1)]
               [c2 (in-string s2)])
      (cond [(char<? c1 c2) (values (add1 xy) yx)]
            [(char>? c1 c2) (values xy (add1 yx))]
            [else (values xy yx)])))
  (define-values [qxy rxy] (quotient/remainder xy 2))
  (define-values [qyx ryx] (quotient/remainder yx 2))
  (cond [(= rxy ryx 1) (+ 2 qxy qyx)]
        [(= rxy ryx 0) (+ qxy qyx)]
        [else -1]))