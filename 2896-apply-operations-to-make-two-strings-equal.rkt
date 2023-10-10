#lang racket

(define/contract (min-operations s1 s2 x)
  (-> string? string? exact-integer? exact-integer?)
  (define l
    (for/list ([(c1 i) (in-indexed (in-string s1))]
               [c2 (in-string s2)]
               #:when (not (char=? c1 c2)))
      i))
  (define (pair-l)
    (define s
      (for/fold ([d-1 0] [d0 0] [p (car l)] #:result d0)
                ([i (in-list (cdr l))])
        (values d0 (max (+ d-1 (- x (- i p))) d0) i)))
    (define n (length l))
    (define-values (q r) (quotient/remainder n 2))
    (if (zero? r)
        (- (* q x) s)
        -1))
  (if (null? l) 0 (pair-l)))

(min-operations "0" "0" 1)
(min-operations "1100011000" "0101001010" 2)
(min-operations "1" "0" 1)
(min-operations "11" "00" 1)
(min-operations "101" "000" 1)
(min-operations "11001011111" "01111000110" 2)
