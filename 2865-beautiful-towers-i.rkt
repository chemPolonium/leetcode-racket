#lang racket

(define/contract (maximum-sum-of-heights maxHeights)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length maxHeights))
  (define (cumsum-heights hs)
    (define v (make-vector n))
    (for/fold ([stack null])
              ([(h i) (in-indexed (in-list hs))])
      (define res-stack
        (memf (lambda (x) (< (car x) h)) stack))
      (if res-stack
          (let ([step-ind (cdar res-stack)])
            (vector-set! v i (+ (vector-ref v step-ind)
                                (* (- i step-ind) h))))
          (vector-set! v i (* (add1 i) h)))
      (cons (cons h i) (or res-stack null)))
    (vector->list v))
  (foldl (lambda (a b c m) (max m (- (+ a b) c)))
         0
         (cumsum-heights maxHeights)
         (reverse (cumsum-heights (reverse maxHeights)))
         maxHeights))

(maximum-sum-of-heights '(5 3 4 1 1))
(maximum-sum-of-heights '(6 5 3 9 2 7))
