#lang racket

(define (longest-square-streak nums)
  (define (square x) (* x x))
  (define m
    (let iter ([l (sort nums >)]
               [s1 (hash)]
               [m 0])
      (if (null? l)
          m
          (let* ([i (car l)]
                 [il (add1 (hash-ref s1 (square i) 0))])
            (iter (cdr l) (hash-set s1 i il) (max m il))))))
  (if (= m 1)
      -1
      m))

(longest-square-streak '[4 3 6 16 8 2])