#lang racket

(define/contract (final-value-after-operations operations)
  (-> (listof string?) exact-integer?)
  (define h (hash "--X" -1 "X--" -1 "++X" 1 "X++" 1))
  (apply + (map (lambda (s) (hash-ref h s)) operations)))