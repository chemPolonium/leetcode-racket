#lang racket

(define (is-circular-sentence sentence)
  (let ([ss (string-split sentence)])
    (for/and ([s0 (in-list ss)]
              [s1 (sequence-append (in-list (cdr ss)) (in-list (list (car ss))))])
      (equal? (string-ref s0 (sub1 (string-length s0))) (string-ref s1 0)))))

(is-circular-sentence "leetcode exercises sound delightful")