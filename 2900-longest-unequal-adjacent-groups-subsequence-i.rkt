#lang racket

(define (get-longest-subsequence words groups)
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

(get-longest-subsequence 3 '("e" "a" "b") '(0 0 1))
(get-longest-subsequence 4 '("a" "b" "c" "d") '(1 0 1 1))
