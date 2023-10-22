#lang racket

(define/contract (max-satisfaction satisfaction)
  (-> (listof exact-integer?) exact-integer?)
  (let loop ([satisfaction (sort satisfaction >)] [liketime 0] [s 0])
    (if (null? satisfaction)
        liketime
        (let ([ns (+ s (car satisfaction))])
          (if (> ns 0)
              (loop (cdr satisfaction) (+ liketime ns) ns)
              liketime)))))

(max-satisfaction '(-1 -8 0 5 -9))
(max-satisfaction '(4 3 2))
(max-satisfaction '(-1 -4 -5))
