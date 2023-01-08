#lang racket

(define/contract (min-operations nums x)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define msum-target (- (apply + nums) x))
  (define v (list->vector nums))
  (define len (vector-length v))
  (define (iter l r msum)
    (cond [(or (and (< msum msum-target) (= r len)) (= l r len)) -1]
          [(< msum msum-target) (iter l (add1 r) (+ msum (vector-ref v r)))]
          [(> msum msum-target) (iter (add1 l) r (- msum (vector-ref v l)))]
          [else (max (- r l) (iter (add1 l) r (- msum (vector-ref v l))))]))
  (if (negative? msum-target)
      -1
      (let ([i (iter 0 0 0)])
        (cond [(nonnegative-integer? i) (- len i)] [else i]))))

; (min-operations '(5 6 7 8 9) 4)

; (min-operations '(5 2 3 1 1) 5)

(min-operations '[8828 9581 49 9818 9974 9869 9991 10000 10000 10000 9999 9993 9904 8819 1231 6309] 134365)