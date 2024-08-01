#lang racket

(define/contract (multiply num1 num2)
  (-> string? string? string?)
  (number->string (* (string->number num1)
                     (string->number num2))))