#lang racket

(define/contract (max-number-of-alloys n k budget composition stock cost)
  (-> exact-integer? exact-integer? exact-integer? (listof (listof exact-integer?)) (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define (num->budget num composition-i)
    (define need (map (λ (x) (* num x)) composition-i))
    (define buy (map (λ (x y) (max 0 (- x y))) need stock))
    (apply + (map * buy cost)))
  (define (search composition-i)
    (let iter ([lo 0] [hi 1000000001])
      (cond [(= lo hi) lo]
            [else
             (define mid (quotient (+ lo hi) 2))
             (define mid-budget (num->budget mid composition-i))
             (cond [(= mid lo) mid]
                   [(> mid-budget budget) (iter lo mid)]
                   [else (iter mid hi)])])))
  (apply max (map search composition)))

(max-number-of-alloys 3 2 15 '((1 1 1) (1 1 10)) '(0 0 0) '(1 2 3))

(max-number-of-alloys 3 2 15 '((1 1 1) (1 1 10)) '(0 0 100) '(1 2 3))

(max-number-of-alloys 2 3 10 '((2 1) (1 2) (1 1)) '(1 1) '(3 5))

(max-number-of-alloys 1 1 0 '((1)) '(77472690) '(1))
