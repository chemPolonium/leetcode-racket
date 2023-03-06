#lang racket

(define/contract (minimum-deletions s)
  (-> string? exact-integer?)
  (define blist
    (let ([b 0])
      (for/list ([c (in-string s)])
        (define new-b (if (char=? c #\b) (add1 b) b))
        (set! b new-b)
        new-b)))
  (define alist
    (let ([a 0])
      (reverse
       (for/list ([c (in-list (reverse (string->list s)))])
         (define new-a (if (char=? c #\a) (add1 a) a))
         (set! a new-a)
         new-a))))
  (sub1 (apply min (map + alist blist))))

(minimum-deletions "aababbab")