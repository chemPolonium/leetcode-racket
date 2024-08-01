#lang racket

(define/contract (crack-safe n k)
  (-> exact-integer? exact-integer? string?)
  (define v (make-vector (expt k (sub1 n)) (sub1 k)))
  (define kn-2 (expt k (- n 2)))
  (define (iter i l)
    (let ([j (vector-ref v i)])
      (cond [(negative-integer? j) l]
            [else
             (vector-set! v i (sub1 j))
             (iter (+ j (* k (remainder i kn-2))) (cons j l))])))
  (list->string (map (lambda (x) (integer->char (+ x 48)))
                     (if (= n 1) (range k) (iter 0 (make-list (sub1 n) 0))))))

(crack-safe 1 2)
(crack-safe 2 3)
(crack-safe 3 3)