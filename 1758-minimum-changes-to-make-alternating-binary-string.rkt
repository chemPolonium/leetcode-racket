#lang racket

(define/contract (min-operations s)
  (-> string? exact-integer?)
  (let-values ([(l m) (for/fold ([i 0] [m 0])
                                ([t (in-cycle (in-list (list #\0 #\1)))]
                                 [c (in-string s)])
                        (values (add1 i) (if (equal? t c) (add1 m) m)))])
    (min m (- l m))))

(min-operations "0100")
(min-operations "10")
(min-operations "1111")
(min-operations "1")