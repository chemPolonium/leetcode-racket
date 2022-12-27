#lang racket

(define/contract (minimum-length s)
  (-> string? exact-integer?)
  (let* ([l (string->list s)]
         [r (reverse l)]
         [len (length l)])
    (let iter ([l l] [r r] [len len] [c #f])
      (cond [(zero? len) 0]
            [(= len -1) 1]
            [(or (null? l) (null? r)) 0]
            [(eq? c (car l)) (iter (cdr l) r (- len 1) c)]
            [(eq? c (car r)) (iter l (cdr r) (- len 1) c)]
            [(eq? (car l) (car r)) (iter (cdr l) (cdr r) (- len 2) (car l))]
            [else len]))))

(minimum-length "aabccabba")
(minimum-length "abbbbbbbbbbbbbbbbbbba")
(minimum-length "bbbbbbbbbbbbbbbbbbbbbbbbbbbabbbbbbbbbbbbbbbccbcbcbccbbabbb")