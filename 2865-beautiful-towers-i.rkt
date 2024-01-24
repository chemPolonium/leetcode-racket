#lang racket

(define/contract (maximum-sum-of-heights maxHeights)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length maxHeights))
  (define (cumsum-heights hs)
    (define v (make-vector n))
    (for/fold ([stack null])
              ([(h i) (in-indexed (in-list hs))])
      (cond [(zero? i)
             (vector-set! v 0 h)
             (list (cons h i))]
            [(< (caar stack) h)
             (vector-set! v i (+ (vector-ref v (sub1 i)) h))
             (cons (cons h i) stack)]
            [else
             (define tail-stack
               (memf (lambda (x)
                       (< (car x) h))
                     stack))
             (if tail-stack
                 (let ([step-ind (cdar tail-stack)])
                   (vector-set! v i (+ (vector-ref v step-ind)
                                       (* (- i step-ind) h))))
                 (vector-set! v i (* (add1 i) h)))
             (cons (cons h i) (or tail-stack null))]))
    (vector->list v))
  (foldl (lambda (a b c m) (max m (- (+ a b) c)))
         0
         (cumsum-heights maxHeights)
         (reverse (cumsum-heights (reverse maxHeights)))
         maxHeights))

(maximum-sum-of-heights '(5 3 4 1 1))
(maximum-sum-of-heights '(6 5 3 9 2 7))
