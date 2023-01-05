#lang racket

; TIME OUT

(define/contract (count-pairs nums low high)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (sequence-count
   (lambda (x) (<= low (bitwise-xor (car x) (cadr x)) high))
   (in-combinations nums 2)))

(count-pairs '(1 4 2 7) 2 6)