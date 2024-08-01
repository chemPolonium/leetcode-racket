#lang racket

(define/contract (num-subarray-bounded-max nums left right)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (define (subs-between a b)
    (let ([n (- b a)])
      (/ (* n (sub1 n)) 2)))
  (define (iter i j l s)
    (cond [(null? l)
           (+ (- s (subs-between j i)) (subs-between -1 i))]
          [(< (car l) left)
           (iter (add1 i) j (cdr l) s)]
          [(<= (car l) right)
           (iter (add1 i) i (cdr l) (- s (subs-between j i)))]
          [else
           (+ (- s (subs-between j i)) (subs-between -1 i)
              (iter 0 -1 (cdr l) 0))]))
  (iter 0 -1 nums 0))

(num-subarray-bounded-max '(2 1 4 3) 2 3)