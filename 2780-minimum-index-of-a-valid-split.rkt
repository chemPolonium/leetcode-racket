#lang racket

(define/contract (minimum-index nums)
  (-> (listof exact-integer?) exact-integer?)
  (define n (length nums))
  (define pivot 0)
  (define h (make-hasheq))
  (for ([i (in-list nums)])
    (hash-update! h i add1 0)
    (when (> (* 2 (hash-ref h i)) n)
      (set! pivot i)))
  (if (and (odd? n) (= (/ (add1 n) 2) (hash-ref h pivot)))
      -1
      (let iter ([i 0] [freq 0] [nums nums])
        (define n-freq (if (= pivot (car nums)) (add1 freq) freq))
        (if (> n-freq (/ (add1 i) 2))
            i
            (iter (add1 i) n-freq (cdr nums))))))

(minimum-index '(1 2 2 2))
