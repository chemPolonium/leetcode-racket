#lang racket

(define/contract (vowel-strings words left right)
  (-> (listof string?) exact-integer? exact-integer? exact-integer?)
  (define subwords (take (drop words left) (- right left -1)))
  (define (vowel? c)
    (memq c '(#\a #\e #\i #\o #\u)))
  (define (vowel-word? s)
    (and (vowel? (string-ref s 0))
         (vowel? (string-ref s (sub1 (string-length s))))))
  (count vowel-word? subwords))
