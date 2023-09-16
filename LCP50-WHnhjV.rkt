#lang racket

(define/contract (give-gem gem operations)
  (-> (listof exact-integer?) (listof (listof exact-integer?)) exact-integer?)
  (define gem-vec (list->vector gem))
  (for ([op (in-list operations)])
    (define a-gem (vector-ref gem-vec (first op)))
    (define give (quotient a-gem 2))
    (vector-set! gem-vec (first op) (- a-gem give))
    (vector-set! gem-vec (second op) (+ (vector-ref gem-vec (second op)) give)))
  (define gem-postop (vector->list gem-vec))
  (- (apply max gem-postop) (apply min gem-postop)))
