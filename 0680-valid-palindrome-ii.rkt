#lang racket

(define/contract (valid-palindrome s)
  (-> string? boolean?)
  (let iter ([i 0] [j (sub1 (string-length s))] [changed false])
    (cond [(<= j i) true]
          [(char=? (string-ref s i) (string-ref s j))
           (iter (add1 i) (sub1 j) changed)]
          [changed false]
          [else
           (or (iter (add1 i) j true)
               (iter i (sub1 j) true))])))

(valid-palindrome "aba")
(valid-palindrome "abca")
(valid-palindrome "abc")
(valid-palindrome "deeee")
