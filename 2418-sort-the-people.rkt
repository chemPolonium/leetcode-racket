#lang racket

(define/contract (sort-people names heights)
  (-> (listof string?) (listof exact-integer?) (listof string?))
  (map car (sort (map cons names heights) > #:key cdr)))