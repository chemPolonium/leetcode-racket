#lang racket

;;; (VECTOR-BINARY-SEARCH <vector> <value> <cmp>)
;;;       -> exact, nonnegative integer or #F
;;;     (CMP <value1> <value2>) -> integer
;;;       positive -> VALUE1 > VALUE2
;;;       zero     -> VALUE1 = VALUE2
;;;       negative -> VALUE1 < VALUE2
;;;   Perform a binary search through VECTOR for VALUE, comparing each
;;;   element to VALUE with CMP.
(define (vector-binary-search vec value [cmp -])
  (let iter ([start 0]
             [end (vector-length vec)])
    (if (= start end)
        #f
        (let* ([i (quotient (+ start end) 2)]
               [comparison (cmp (vector-ref vec i) value)])
          (cond [(positive? comparison) (iter start i)]
                [(negative? comparison) (iter (add1 i) end)]
                [else i])))))

(vector-binary-search #(1 3 5 7 9) 0)
(vector-binary-search #(1 3 5 7 9) 3)
(vector-binary-search #(1 3 5 7 9) 5)
(vector-binary-search #(1 3 5 7 9) 10)

; (define v (build-vector 10000000 identity))
; (time (for/and ([i (in-range 10000000)])
;         (vector-bs v i)))

(define (vector-lower-bound vec value [cmp -])
  (let iter ([current-ind 0]
             [hl-dis (vector-length vec)])
    (if (positive? hl-dis)
        (let* ([step (quotient hl-dis 2)]
               [next-ind (+ current-ind step)]
               [comparison (cmp (vector-ref vec next-ind) value)])
          (if (negative? comparison)
              (iter (add1 next-ind) (- hl-dis step 1))
              (iter current-ind step)))
        current-ind)))

(vector-lower-bound #(1 3 5 7 9) 0)
(vector-lower-bound #(1 3 5 7 9) 3)
(vector-lower-bound #(1 3 5 7 9) 4)
(vector-lower-bound #(1 3 5 7 9) 10)

(define (vector-upper-bound vec value [cmp -])
  (let iter ([current-ind 0]
             [hl-dis (vector-length vec)])
    (if (positive? hl-dis)
        (let* ([step (quotient hl-dis 2)]
               [next-ind (+ current-ind step)]
               [comparison (cmp (vector-ref vec next-ind) value)])
          (if (positive? comparison)
              (iter current-ind step)
              (iter (add1 next-ind) (- hl-dis step 1))))
        current-ind)))

(vector-upper-bound #(1 3 5 7 9) 0)
(vector-upper-bound #(1 3 5 7 9) 3)
(vector-upper-bound #(1 3 5 7 9) 4)
(vector-upper-bound #(1 3 5 7 9) 10)