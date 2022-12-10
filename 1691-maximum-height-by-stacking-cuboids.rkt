#lang racket

(define (max-height cuboids)
  (apply max
         (for/fold ([ls null]
                    #:result (map third ls))
                   ([c (sort (map (lambda (c) (sort c <)) cuboids) <
                             #:key (lambda (x) (apply + (map * x '(1 1000 1000000)))))])
           (cons (let iter ([l ls] [m (third c)])
                   (if (null? l) (list-set c 2 m)
                       (match (car l)
                         [(list cs1 cs2 cs3)
                          (if (and (<= cs1 (first c)) (<= cs2 (second c)))
                              (iter (cdr l) (max m (+ cs3 (third c))))
                              (iter (cdr l) m))])))
                 ls))))

(define cuboids '[[29 59 36] [12 13 97] [49 86 43] [9 57 50] [97 19 10] [17 92 69] [92 36 15] [16 63 8] [94 24 78] [52 11 39] [48 61 57] [15 44 79] [6 69 98] [30 70 41] [23 17 33] [85 86 12] [13 75 98] [75 30 30] [89 18 27] [94 83 81]])

(max-height cuboids)

; (sort (map (lambda (c) (sort c <)) cuboids) < #:key third)