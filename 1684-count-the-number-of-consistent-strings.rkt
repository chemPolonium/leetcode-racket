#lang racket

(define (consistent-string? allowed str)
  (list? (andmap ((curryr member) (string->list allowed))
                 (string->list str))))

(define/contract (count-consistent-strings allowed words)
  (-> string? (listof string?) exact-integer?)
  (count (curry consistent-string? allowed)
         words))

; (define/contract (count-consistent-strings allowed words)
;   (-> string? (listof string?) exact-integer?)
;   (length (filter (curry consistent-string? allowed)
;                   words)))

(count-consistent-strings "ab" '("ad" "bd" "aaab" "baa" "badab"))
(count-consistent-strings "abc" '("a" "b" "c" "ab" "ac" "bc" "abc"))