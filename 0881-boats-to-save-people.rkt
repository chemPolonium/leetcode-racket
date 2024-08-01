#lang racket

(define/contract (num-rescue-boats people limit)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define sorted-people (sort people <))
  (define vec (list->vector sorted-people))
  (define pairs
    (let iter ([i 0] [j (sub1 (vector-length vec))] [c 0])
      (define vec-i (vector-ref vec i))
      (define vec-j (vector-ref vec j))
      (define sum-ij (+ vec-i vec-j))
      (cond [(> vec-i limit) c]
            [(>= i j) c]
            [(> sum-ij limit) (iter i (sub1 j) c)]
            [else (iter (add1 i) (sub1 j) (add1 c))])))
  (- (vector-length vec) pairs))

(num-rescue-boats '(1 2) 3)
(num-rescue-boats '(3 2 2 1) 3)
(num-rescue-boats '(3 5 3 4) 5)
