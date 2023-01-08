#lang racket

(define/contract (prefix-count words pref)
  (-> (listof string?) string? exact-integer?)
  (count (lambda (s) (string-prefix? s pref)) words))