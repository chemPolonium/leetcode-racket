#lang racket

(define/contract (min-groups-for-valid-assignment nums)
  (-> (listof exact-integer?) exact-integer?)
  (define h (make-hasheq))
  (for ([n (in-list nums)])
    (hash-update! h n add1 0))
  (define c (hash-values h))
  ; group member num
  (define (gmn? n k)
    (define-values (q r) (quotient/remainder n k))
    (or (zero? r)
        (<= (- k r) (add1 q))))
  (define (gmn-all? k)
    (andmap (λ (x) (gmn? x k)) c))
  (define minc (apply min c))
  (define gmn
    (let loop ([i 1] [p 0])
      (define-values (q r) (quotient/remainder minc i))
      (define k (if (zero? r) q (add1 q)))
      (cond [(= k p) (loop (add1 i) p)]
            [(and (zero? r) (gmn-all? (add1 k))) (add1 k)]
            [(gmn-all? k) k]
            [(= k 2) 2]
            [else (loop (add1 i) k)])))
  (apply + (map (λ (x) (ceiling (/ x gmn))) c)))

(min-groups-for-valid-assignment '(3 2 3 2 3))
(min-groups-for-valid-assignment '(10 10 10 3 1 1))
(min-groups-for-valid-assignment '(2 1 1 1 2 1 2 1 3 3 3 2 1 3 3))
