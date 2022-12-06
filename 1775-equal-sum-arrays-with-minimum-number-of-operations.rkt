#lang racket

(define (min-op nums1 nums2 s)
  (define (iter l res i)
    (cond [(<= res 0) i]
          [(null? l) -1]
          [else (iter (cdr l) (- res (car l)) (add1 i))]))
  (let ([l (sort (append (map (lambda (x) (- 6 x)) nums1)
                         (map sub1 nums2)) >)])
    (iter l s 0)))

(define/contract (min-operations nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let ([s1 (apply + nums1)]
        [s2 (apply + nums2)])
    (if (< s1 s2)
        (min-op nums1 nums2 (- s2 s1))
        (min-op nums2 nums1 (- s1 s2)))))

; (min-operations '(1 2 3 4 5 6) '(1 1 2 2 2 2))

(min-operations '[5 2 1 5 2 2 2 2 4 3 3 5] '[1 4 5 5 6 3 1 3 3])