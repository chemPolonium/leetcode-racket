#lang racket

(define/contract (add-spaces s spaces)
  (-> string? (listof exact-integer?) string?)
  (define s1 (make-string (+ (string-length s) (length spaces)) #\ ))
  (for/fold ([j 0] [ss spaces])
            ([i (in-range (string-length s1))])
    (cond [(or (null? ss) (< j (car ss)))
           (string-set! s1 i (string-ref s j))
           (values (add1 j) ss)]
          [else
           (values j (cdr ss))]))
  s1)

(add-spaces "LeetcodeHelpsMeLearn" '(8 13 15))
