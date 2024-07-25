#lang racket

(define/contract (minimum-operations num)
  (-> string? exact-integer?)
  (define n (string-length num))
  (let iter ([i (sub1 n)]
             [has0 false]
             [has5 false])
    (if (= i -1)
        (if has0 (sub1 n) n)
        (case (string-ref num i)
          [(#\0) (if has0 (- n i 2) (iter (sub1 i) true has5))]
          [(#\5) (if has0 (- n i 2) (iter (sub1 i) has0 true))]
          [(#\2 #\7) (if has5 (- n i 2) (iter (sub1 i) has0 has5))]
          [else (iter (sub1 i) has0 has5)]))))

(minimum-operations "2245047")
(minimum-operations "2908305")
