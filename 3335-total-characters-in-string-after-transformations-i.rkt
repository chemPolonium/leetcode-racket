#lang racket

(define/contract (length-after-transformations s t)
  (-> string? exact-integer? exact-integer?)
  (define v (make-vector 26))
  (for ([c (in-string s)])
    (define i (- (char->integer c) 97))
    (vector-set! v i (add1 (vector-ref v i))))
  (let iter ([n 0] [pa 0])
    (if (= n t)
        (remainder (apply + (vector->list v)) 1000000007)
        (let ([pz (remainder (+ 25 pa) 26)]
              [pb (remainder (add1 pa) 26)])
          (vector-set! v pa (+ (vector-ref v pa) (vector-ref v pz)))
          (iter (add1 n) pz)))))

(length-after-transformations "abcyy" 2)
