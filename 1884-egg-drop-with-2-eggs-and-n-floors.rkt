#lang racket

(define/contract (two-egg-drop n)
  (-> exact-integer? exact-integer?)
  (exact-ceiling (/ (sub1 (sqrt (add1 (* 8 n)))) 2)))
