#lang racket

; Why this works?
; JIT of racket is awesome
(define/contract (get-good-indices variables target)
  (-> (listof (listof exact-integer?)) exact-integer? (listof exact-integer?))
  (define (target? a b c m)
    (= target (remainder (expt (remainder (expt a b) 10) c) m)))
  (for/list ([i (in-naturals)]
             [v (in-list variables)]
             #:when (apply target? v))
    i))
