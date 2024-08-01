#lang racket

(define/contract (trap height)
  (-> (listof exact-integer?) exact-integer?)
  (define (scan-max l)
    (let ([m 0])
      (for/list ([x (in-list l)])
        (set! m (max m x))
        m)))
  (- (apply + (map min (scan-max height) (reverse (scan-max (reverse height)))))
     (apply + height)))