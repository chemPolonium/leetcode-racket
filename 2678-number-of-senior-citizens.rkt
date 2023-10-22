#lang racket

(define/contract (count-seniors details)
  (-> (listof string?) exact-integer?)
  (count (λ (s) (> (string->number (substring s 11 13)) 60)) details))
