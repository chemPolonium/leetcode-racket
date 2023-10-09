#lang racket

(define/contract (sum-distance nums s d)
  (-> (listof exact-integer?) string? exact-integer? exact-integer?)
  (define p
    (for/list ([i (in-list nums)]
               [c (in-string s)])
      (if (char=? c #\L)
          (- i d)
          (+ i d))))
  (define sp (sort p <))
  (define n (length sp))
  (define dissum
    (for/sum ([(p i) (in-indexed (in-list sp))])
      (* (- i (- n i 1)) p)))
  (remainder dissum 1000000007))

(sum-distance '(-2 0 2) "RLL" 3)
