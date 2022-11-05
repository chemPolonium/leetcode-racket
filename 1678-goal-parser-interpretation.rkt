#lang racket

(define/contract (interpret command)
  (-> string? string?)
  (foldl (lambda (from to str)
           (string-replace str from to))
         command
         '("()" "(al)")
         '("o" "al")))

(interpret "G()()()()(al)")