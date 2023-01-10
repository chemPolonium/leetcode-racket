#lang racket

(define (vector-update! vec pos updater)
  (vector-set! vec pos (updater (vector-ref vec pos))))

(define (char->int c)
  (- (char->integer c) 48))

(define/contract (digit-count num)
  (-> string? boolean?)
  (define v (make-vector 10))
  (for ([c (in-string num)])
    (vector-update! v (char->int c) add1))
  (for/and ([c (in-string num)] [i (in-naturals)])
    (= (vector-ref v i) (char->int c))))

(digit-count "1210")
(digit-count "030")