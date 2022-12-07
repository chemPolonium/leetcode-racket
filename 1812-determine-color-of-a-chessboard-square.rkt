#lang racket

(define/contract (square-is-white coordinates)
  (-> string? boolean?)
  (match (map char->integer (string->list coordinates)) [(list c i) (odd? (+ c i))]))

(square-is-white "h3")