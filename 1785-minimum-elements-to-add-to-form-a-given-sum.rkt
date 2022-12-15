#lang racket

(define/contract (min-elements nums limit goal)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (ceiling (/ (abs (- goal (apply + nums))) limit)))

(min-elements '(1 -1 1) 3 -4)