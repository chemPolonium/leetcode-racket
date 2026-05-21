#lang racket

(define/contract (longest-common-prefix arr1 arr2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define (d x [a '()])
    (if (zero? x)
        a
        (let-values ([(q r) (quotient/remainder x 10)])
          (d q (cons r a)))))
  (define v (make-vector 10 #f))
  (for ([path (in-list arr1)])
    (for/fold ([c v])
              ([d (in-list (d path))])
      (cond [(vector-ref c d)]
            [else
             (vector-set! c d (make-vector 10 #f))
             (vector-ref c d)])))
  (define (? q)
    (define dq (d q))
    (let iter ([dq dq] [c v] [a 0])
      (cond [(null? dq) a]
            [(vector-ref c (car dq))
             (iter (cdr dq) (vector-ref c (car dq)) (add1 a))]
            [else a])))
  (for/fold ([a 0])
            ([q (in-list arr2)])
    (max a (? q))))

(longest-common-prefix '(1 10 100) '(1000))
(longest-common-prefix '(1 2 3) '(4 4 4))
