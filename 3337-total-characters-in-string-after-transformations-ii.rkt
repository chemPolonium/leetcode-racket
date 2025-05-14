#lang racket

(define/contract (length-after-transformations s t nums)
  (-> string? exact-integer? (listof exact-integer?) exact-integer?)
  (define mod 1000000007)
  (define v (make-vector 26))
  (for ([c (in-string s)])
    (define i (- (char->integer c) 97))
    (vector-set! v i (add1 (vector-ref v i))))
  (define vm (build-list 26 (lambda (_) (make-vector 26))))
  (for ([(num i) (in-indexed (in-list nums))]
        [vmi (in-list vm)])
    (for ([j (in-range (add1 i) (+ i 1 num))])
      (vector-set! vmi (remainder j 26) 1)))
  (define m
    (for/list ([i (in-range 26)])
      (map (lambda (vmi) (vector-ref vmi i)) vm)))
  (define (mat* a b)
    (define b-tran (apply map list b))
    (map (lambda (row)
           (map (lambda (col)
                  (remainder (foldl + 0 (map * row col)) mod))
                b-tran))
         a))
  (define (mat-expt m n)
    (if (= n 1)
        m
        (let-values ([(q r) (quotient/remainder n 2)])
          (let* ([m1 (mat-expt m q)]
                 [m2 (mat* m1 m1)])
            (if (= r 1)
                (mat* m m2)
                m2)))))
  (define res
    (mat* (mat-expt m t) (map list (vector->list v))))
  (remainder (apply + (map car res)) mod))

(length-after-transformations "abbyy" 2 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2))
