#lang racket

(define/contract (ways-to-make-fair nums)
  (-> (listof exact-integer?) exact-integer?)
  (define-values (esum osum)
    (for/fold ([esum 0] [osum 0])
              ([(n i) (in-indexed (in-list nums))])
      (if (even? i) (values (+ n esum) osum) (values esum (+ n osum)))))
  (for/fold ([esum1 0] [osum1 0] [esum2 esum] [osum2 osum] [c 0] #:result c)
            ([(n i) (in-indexed (in-list nums))])
    (define-values (nesum1 nosum1 nesum2 nosum2)
      (cond [(even? i) (values (+ esum1 n) osum1 (- esum2 n) osum2)]
            [else (values esum1 (+ osum1 n) esum2 (- osum2 n))]))
    (values nesum1 nosum1 nesum2 nosum2
            (if (= (+ osum1 nesum2) (+ esum1 nosum2)) (add1 c) c))))

(ways-to-make-fair '(2 1 6 4))