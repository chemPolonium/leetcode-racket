#lang racket

(define/contract (percentage-letter s letter)
  (-> string? char? exact-integer?)
  (exact-floor
   (* 100
      (/ (count (lambda (x) (char=? x letter))
                (string->list s))
         (string-length s)))))
