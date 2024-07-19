#lang racket

(define/contract (minimum-levels possible)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length possible))
  (define simple (apply + possible))
  (define hard (- n simple))
  (define all-score (- simple hard))
  (let iter ([possible possible] [s 0] [i 1])
    (if (= i n)
        -1
        (let* ([p (car possible)]
               [ns (+ s (if (zero? p) -1 1))])
          (if (> (* 2 ns) all-score)
              i
              (iter (cdr possible) ns (add1 i)))))))
