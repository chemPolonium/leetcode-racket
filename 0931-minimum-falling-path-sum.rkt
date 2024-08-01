#lang racket

(define (min-falling-path-sum matrix)
  (let iter ([matrix (cdr matrix)] [min-sum (car matrix)])
    (if (null? matrix)
        (apply min min-sum)
        (iter
         (cdr matrix)
         (for/list ([a1 (sequence-append (in-value 114514) (in-list min-sum))]
                    [a2 (in-list min-sum)]
                    [a3 (sequence-append (in-list (cdr min-sum)) (in-value 114514))]
                    [b (in-list (car matrix))])
           (+ b (min a1 a2 a3)))))))