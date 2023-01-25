#lang racket

(define/contract (get-smallest-string n k)
  (-> exact-integer? exact-integer? string?)
  (define z-num (quotient (- k n) 25))
  (define a-num (max 0 (- n z-num 1)))
  (define c-num (- n z-num a-num))
  (define c (- k a-num (* z-num 26)))
  (string-append (make-string a-num #\a)
                 (make-string c-num (integer->char (+ c 96)))
                 (make-string z-num #\z)))

(get-smallest-string 1 26)