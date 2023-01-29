#lang racket

(define/contract (count-asterisks s)
  (-> string? exact-integer?)
  (for/fold ([has-bar #t] [n 0] #:result n)
            ([c (in-string s)])
    (cond [(char=? c #\|) (values (not has-bar) n)]
          [(and has-bar (char=? c #\*)) (values has-bar (add1 n))]
          [else (values has-bar n)])))

(count-asterisks "l|*e*et|c**o|*de|")