#lang racket

(define/contract (number-of-pairs nums1 nums2 k)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer? exact-integer?)
  (define h1 (make-hash))
  (define h2 (make-hash))
  (for ([n1i (in-list nums1)])
    (hash-update! h1 n1i add1 0))
  (for ([n2i (in-list nums2)])
    (hash-update! h2 n2i add1 0))
  (define max1 (apply max nums1))
  (for*/sum ([(a cnt) (in-hash h2)]
             [b (in-inclusive-range (* a k) max1 (* a k))])
    (* (hash-ref h2 b 0) cnt)))
