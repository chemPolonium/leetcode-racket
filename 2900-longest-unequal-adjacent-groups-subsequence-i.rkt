#lang racket

(define/contract (get-words-in-longest-subsequence n words groups)
  (-> exact-integer? (listof string?) (listof exact-integer?) (listof string?))
  (define words-vec (list->vector words))
  (define g
    (let iter ([gi (car groups)] [i 0] [groups (cdr groups)] [acc '(0)])
      (cond [(null? groups)
             (reverse acc)]
            [(= gi (car groups))
             (iter gi (add1 i) (cdr groups) acc)]
            [else
             (iter (car groups) (add1 i) (cdr groups) (cons (add1 i) acc))])))
  (map (λ (i) (vector-ref words-vec i)) g))

(get-words-in-longest-subsequence 3 '("e" "a" "b") '(0 0 1))
(get-words-in-longest-subsequence 4 '("a" "b" "c" "d") '(1 0 1 1))
