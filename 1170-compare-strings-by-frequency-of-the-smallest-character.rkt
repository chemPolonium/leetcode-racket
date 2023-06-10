#lang racket

(define/contract (num-smaller-by-frequency queries words)
  (-> (listof string?) (listof string?) (listof exact-integer?))
  (define (f s)
    (for/fold ([min-c #\z] [min-c-count 0] #:result min-c-count)
              ([c (in-string s)])
      (cond [(char=? c min-c) (values c (add1 min-c-count))]
            [(char<? c min-c) (values c 1)]
            [else (values min-c min-c-count)])))
  (define v (make-vector 11))
  (for ([i (in-list (map f words))] #:unless (zero? i))
    (vector-set! v (sub1 i) (add1 (vector-ref v (sub1 i)))))
  (for ([i (in-inclusive-range 8 0 -1)])
    (vector-set! v i (+ (vector-ref v i) (vector-ref v (add1 i)))))
  (map (lambda (x) (vector-ref v (f x))) queries))

(num-smaller-by-frequency
 '["bba" "abaaaaaa" "aaaaaa" "bbabbabaab" "aba" "aa" "baab" "bbbbbb" "aab" "bbabbaabb"]
 '["aaabbb" "aab" "babbab" "babbbb" "b" "bbbbbbbbab" "a" "bbbbbbbbbb" "baaabbaab" "aa"])