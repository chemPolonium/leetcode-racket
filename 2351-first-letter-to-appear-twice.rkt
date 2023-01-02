#lang racket

(define/contract (repeated-character s)
  (-> string? char?)
  (let iter ([l (string->list s)] [s (set)])
    (let ([c (car l)])
      (if (set-member? s c)
          c
          (iter (cdr l) (set-add s c))))))

(repeated-character "abccbaacz")