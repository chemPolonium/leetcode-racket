#lang racket

(define/contract (find-the-longest-balanced-substring s)
  (-> string? exact-integer?)
  (for/fold ([n0 0] [n1 0] [m 0] #:result (* 2 m))
            ([c (in-string s)])
    (cond [(and (char=? c #\0) (zero? n1))
           (values (add1 n0) 0 m)]
          [(char=? c #\0)
           (values 1 0 m)]
          [else
           (let ([n1 (add1 n1)])
             (values n0 n1 (max m (min n0 n1))))])))

(find-the-longest-balanced-substring "")
(find-the-longest-balanced-substring "01000111")
