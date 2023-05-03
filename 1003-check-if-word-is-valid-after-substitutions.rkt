#lang racket

(define/contract (is-valid s)
  (-> string? boolean?)
  (for/fold ([stack empty] #:result (empty? stack))
            ([c (in-string s)])
    (cond [(empty? stack) (list c)]
          [(and (char=? c #\b) (char=? (car stack) #\a)) (cons #\d (cdr stack))]
          [(and (char=? c #\c) (char=? (car stack) #\d)) (cdr stack)]
          [else (cons c stack)])))