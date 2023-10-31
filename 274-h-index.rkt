#lang racket

(define/contract (h-index citations)
  (-> (listof exact-integer?) exact-integer?)
  (let loop ([i 0]
             [citations (sort citations >)])
    (cond [(null? citations) i]
          [(> (car citations) i)
           (loop (add1 i) (cdr citations))]
          [else i])))

(h-index '(0))
(h-index '(1))
(h-index '(1 1 3))
