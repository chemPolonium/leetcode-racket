#lang racket

(define/contract (min-sum nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define s1 (apply + nums1))
  (define s2 (apply + nums2))
  (define c1 (count zero? nums1))
  (define c2 (count zero? nums2))
  (cond [(and (zero? c1) (< s1 (+ s2 c2))) -1]
        [(and (zero? c2) (< s2 (+ s1 c1))) -1]
        [else (max (+ s1 c1) (+ s2 c2))]))

(min-sum '(3 2 0 1 0) '(6 5 0))
