#lang racket

(define/contract (split-array-same-average nums)
  (-> (listof exact-integer?) boolean?)
  (define (f return)
    (define (wrap)
      (define n (length nums))
      (define m (quotient n 2))
      (define s (apply + nums))
      (when (andmap (lambda (i)
                      (positive? (remainder (* s i) n)))
                    (range 1 (add1 m)))
        (return false))
      (define dp (make-vector (add1 m) (set)))
      (vector-set! dp 0 (set-add (vector-ref dp 0) 0))
      (for-each
       (lambda (num)
         (for-each
          (lambda (i)
            (for-each
             (lambda (x)
               (let ([curr (+ x num)])
                 (when (= (* curr n) (* s i))
                   (return true))
                 (vector-set! dp i (set-add (vector-ref dp i) curr))))
             (set->list (vector-ref dp (sub1 i)))))
          (range m 0 -1)))
       nums)
      false)
    (wrap))
  (call/cc f))

(split-array-same-average '(1 2 3 4 5 6 7 8))