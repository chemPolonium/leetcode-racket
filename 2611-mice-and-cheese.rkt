#lang racket

(define/contract (mice-and-cheese reward1 reward2 k)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer? exact-integer?)
  (+ (apply + reward2) (apply + (take (sort (map - reward1 reward2) >=) k))))

(mice-and-cheese '(1 1 3 4) '(4 4 1 1) 2)