#lang racket

(define/contract (min-number nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define i (set-intersect nums1 nums2))
  (if (pair? i)
      (apply min i)
      (min (+ (apply min nums2) (* 10 (apply min nums1)))
           (+ (apply min nums1) (* 10 (apply min nums2))))))
