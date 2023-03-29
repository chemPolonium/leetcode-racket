#lang racket

(define/contract (count-vowel-strings n)
  (-> exact-integer? exact-integer?)
  (let iter ([l '(1 1 1 1 1)] [n n])
    (if (= n 1)
        (apply + l)
        (match l
          [(list a b c d e)
           (iter (list a (+ a b) (+ a b c) (+ a b c d) (+ a b c d e))
                 (sub1 n))]))))

(count-vowel-strings 33)