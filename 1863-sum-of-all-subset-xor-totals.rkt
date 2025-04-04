#lang racket

(define/contract (subset-xor-sum nums)
  (-> (listof exact-integer?) exact-integer?)
  (* (expt 2 (sub1 (length nums))) (apply bitwise-ior nums)))
