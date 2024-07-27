#lang racket

(define/contract (get-smallest-string s k)
  (-> string? exact-integer? string?)
  (list->string
   (let recr ([sl (string->list s)] [k k])
     (cond [(null? sl) null]
           [(zero? k) sl]
           [else
            (define i (char->integer (car sl)))
            (define dis1 (min (- i 97) (- 123 i)))
            (if (<= dis1 k)
                (cons #\a (recr (cdr sl) (- k dis1)))
                (cons (integer->char (- i k)) (cdr sl)))]))))
