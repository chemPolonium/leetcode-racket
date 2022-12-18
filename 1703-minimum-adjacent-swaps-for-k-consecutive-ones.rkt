#lang racket

(require data/gvector)

(define/contract (min-moves nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define g (gvector))
  (define preSum (gvector 0))
  (for ([(num i) (in-indexed (in-list nums))])
    (when (= num 1)
      (gvector-add! g (- i (gvector-count g)))
      (gvector-add! preSum (+ (gvector-ref preSum (sub1 (gvector-count preSum)))
                              (gvector-ref g (sub1 (gvector-count g)))))))
  (for/fold ([res 114514114514])
            ([i (in-range (add1 (- (gvector-count g) k)))])
    (let-values ([(k/2 k%2) (quotient/remainder k 2)])
      (let* ([mid (+ i k/2)]
             [r (gvector-ref g mid)])
        (min res (+ (* r (- 1 k%2))
                    (- (gvector-ref preSum (+ i k))
                       (gvector-ref preSum (add1 mid)))
                    (- (gvector-ref preSum i)
                       (gvector-ref preSum mid))))))))

(min-moves '(1 0 0 1 0 1) 2)