#lang racket

(define special-chars '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\+))

(define (char-special? c)
  (pair? (memq c special-chars)))

(define/contract (strong-password-checker-ii password)
  (-> string? boolean?)
  (let iter ([fs (list char-upper-case? char-lower-case? char-numeric? char-special?)]
             [l (string->list password)]
             [prev false]
             [i 0])
    (cond [(null? l) (and (null? fs) (>= i 8))]
          [else
           (define c (car l))
           (define rfs (remf (lambda (f) (f c)) fs))
           (and (nand prev (char=? c prev))
                (iter rfs (cdr l) c (add1 i)))])))

(strong-password-checker-ii "-Aa1a1a1")