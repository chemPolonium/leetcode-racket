#lang racket

(define (vector-update! vec pos updater)
  (vector-set! vec pos (updater (vector-ref vec pos))))

(define/contract (arithmetic-triplets nums diff)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define rem-count (make-vector diff))
  (define i-rem-count (make-vector diff 0))
  (define q-rem-last (make-vector diff -2))
  (for ([n (in-list nums)])
    (define-values (q r) (quotient/remainder n diff))
    (cond [(= (sub1 q) (vector-ref q-rem-last r))
           (cond [(= 2 (vector-ref i-rem-count r))
                  (vector-update! rem-count r add1)]
                 [else
                  (vector-set! i-rem-count r 2)])]
          [else
           (vector-set! i-rem-count r 1)])
    (vector-set! q-rem-last r q))
  (for/sum ([v (in-vector rem-count)])
    v))

(arithmetic-triplets '(0 1 4 6 7 10) 3)