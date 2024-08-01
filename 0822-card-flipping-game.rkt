#lang racket

(define/contract (flipgame fronts backs)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define v (make-vector 2001))
  (define s (mutable-seteq))
  (for ([f (in-list fronts)]
        [b (in-list backs)])
    (cond [(= f b) (vector-set! v f 1)]
          [else
           (set-add! s f)
           (set-add! s b)]))
  (let iter ([l (sort (set->list s) <)])
    (cond [(null? l) 0]
          [(zero? (vector-ref v (car l))) (car l)]
          [else (iter (cdr l))])))

(flipgame '(1 2 4 4 7) '(1 3 4 1 3))
