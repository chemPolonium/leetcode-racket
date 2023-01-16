#lang racket

(define/contract (maximum-value strs)
  (-> (listof string?) exact-integer?)
  (let* ([l (lambda (s) (if (regexp-match? #rx"[a-z]" s)
                           (string-length s)
                           (string->number s)))]
         [ls (map l strs)])
    (apply max ls)))