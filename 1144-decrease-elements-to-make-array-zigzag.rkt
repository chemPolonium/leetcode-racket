#lang racket

(define/contract (moves-to-make-zigzag nums)
  (-> (listof exact-integer?) exact-integer?)
  (min (for/sum ([l1 (in-slice 2 (in-list nums))]
                 [l2 (in-slice 2 (sequence-append (in-list (rest nums)) (in-list '(114514))))]
                 #:break (< (length l2) 2))
         (define a1 (first l1))
         (define a2 (first l2))
         (define a3 (second l2))
         (max 0 (- a2 (sub1 (min a1 a3)))))
       (for/sum ([l1 (in-slice 2 (in-list (cons 114514 nums)))]
                 [l2 (in-slice 2 (sequence-append (in-list nums) (in-list '(114514))))]
                 #:break (< (length l2) 2))
         (define a1 (first l1))
         (define a2 (first l2))
         (define a3 (second l2))
         (max 0 (- a2 (sub1 (min a1 a3)))))))

(moves-to-make-zigzag '(1 2 3))
(moves-to-make-zigzag '(9 6 1 6 2))
