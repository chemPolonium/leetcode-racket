#lang racket

; (define/contract (add-strings num1 num2)
;   (-> string? string? string?)
;   (number->string (+ (string->number num1) (string->number num2))))

(define/contract (add-strings num1 num2)
  (-> string? string? string?)
  (define (int-char->int c)
    (- (char->integer c) (char->integer #\0)))
  (define (int->int-char i)
    (integer->char (+ i (char->integer #\0))))
  (let iter ([l null] [i (sub1 (string-length num1))] [j (sub1 (string-length num2))] [add 0])
    (cond [(and (negative? i) (negative? j) (zero? add))
           (list->string (map int->int-char l))]
          [else
           (define ni (if (negative? i) 0 (int-char->int (string-ref num1 i))))
           (define nj (if (negative? j) 0 (int-char->int (string-ref num2 j))))
           (define-values (q r) (quotient/remainder (+ ni nj add) 10))
           (iter (cons r l) (sub1 i) (sub1 j) q)])))

(add-strings "11" "123")
