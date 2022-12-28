#lang racket

(define/contract (two-out-of-three nums1 nums2 nums3)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?) (listof exact-integer?))
  (let ([s1 (list->seteq nums1)] [s2 (list->seteq nums2)] [s3 (list->seteq nums3)])
    (set->list (set-union (set-intersect s1 s2) (set-intersect s1 s3) (set-intersect s2 s3)))))