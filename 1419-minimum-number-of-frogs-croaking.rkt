#lang racket

(define/contract (min-number-of-frogs croakOfFrogs)
  (-> string? exact-integer?)
  (define v (make-vector 5))
  (define invalid-bool false)
  (define current-frog-num 0)
  (define max-frog-num 0)
  (for ([c (in-string croakOfFrogs)]
        #:break invalid-bool)
    (match c
      [#\c (vector-set! v 0 (add1 (vector-ref v 0)))
           (set! current-frog-num (add1 current-frog-num))
           (set! max-frog-num (max max-frog-num current-frog-num))]
      [#\r (cond [(zero? (vector-ref v 0))
                  (set! invalid-bool #t)]
                 [else
                  (vector-set! v 1 (add1 (vector-ref v 1)))
                  (vector-set! v 0 (sub1 (vector-ref v 0)))])]
      [#\o (cond [(zero? (vector-ref v 1))
                  (set! invalid-bool #t)]
                 [else
                  (vector-set! v 2 (add1 (vector-ref v 2)))
                  (vector-set! v 1 (sub1 (vector-ref v 1)))])]
      [#\a (cond [(zero? (vector-ref v 2))
                  (set! invalid-bool #t)]
                 [else
                  (vector-set! v 3 (add1 (vector-ref v 3)))
                  (vector-set! v 2 (sub1 (vector-ref v 2)))])]
      [#\k (cond [(zero? (vector-ref v 3))
                  (set! invalid-bool #t)]
                 [else
                  (set! current-frog-num (sub1 current-frog-num))
                  (vector-set! v 3 (sub1 (vector-ref v 3)))])]))
  (if (and (not invalid-bool)
           (andmap (λ (x) (zero? (vector-ref v x))) (range 4)))
      max-frog-num
      -1))

(min-number-of-frogs "croakcroak")