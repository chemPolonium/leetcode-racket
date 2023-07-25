#lang racket

(require data/heap)

(define/contract (halve-array nums)
  (-> (listof exact-integer?) exact-integer?)
  (define h (make-heap >))
  (define s (apply + nums))
  (define ss 0)
  (heap-add-all! h nums)
  (do ([ans 0 (add1 ans)]) ((>= ss (/ s 2)) ans)
    (let ([a (heap-min h)])
      (set! ss (+ ss (/ a 2)))
      (heap-remove-min! h)
      (heap-add! h (/ a 2)))))
