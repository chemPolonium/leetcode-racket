#lang racket

(define/contract (largest-altitude gain)
  (-> (listof exact-integer?) exact-integer?)
  (for/fold ([acc 0] [hi 0] #:result hi) ([x gain])
    (let ([nacc (+ acc x)]) (values nacc (max hi nacc)))))