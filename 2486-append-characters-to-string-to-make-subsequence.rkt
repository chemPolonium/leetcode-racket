#lang racket

(define/contract (append-characters s t)
  (-> string? string? exact-integer?)
  (define (iter s1 s2 res)
    (cond [(and (null? s1) (null? s2)) res]
          [(null? s1) (iter null (cdr s2) (add1 res))]
          [(null? s2) 0]
          [(equal? (car s1) (car s2)) (iter (cdr s1) (cdr s2) res)]
          [else (iter (cdr s1) s2 res)]))
  (iter (string->list s) (string->list t) 0))

(append-characters "coaching" "coding")
(append-characters "abcde" "a")
(append-characters "z" "abcde")