#lang racket

(define/contract (valid-path n edges source destination)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer? exact-integer? boolean?)

  (define fa (apply vector (range n)))
  (define (find x)
    (unless (= x (vector-ref fa x))
      (vector-set! fa x (find (vector-ref fa x))))
    (vector-ref fa x))
  (define (merge from to)
    (vector-set! fa (find from) (find to)))

  (for ([e (in-list edges)])
    (merge (first e) (second e)))

  (= (find source) (find destination)))

(valid-path 3 '((0 1) (1 2) (2 0)) 0 2)