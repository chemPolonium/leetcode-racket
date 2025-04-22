#lang racket

(require math/number-theory)

(define/contract (ideal-arrays n maxValue)
  (-> exact-integer? exact-integer? exact-integer?)
  (define v (make-vector (add1 maxValue) '(1)))
  (define (list-combine l1 l2 [i 0])
    (cond [(or (= i n) (and (null? l1) (null? l2))) null]
          [(null? l1) l2]
          [(null? l2) l1]
          [else (cons (+ (car l1) (car l2))
                      (list-combine (cdr l1) (cdr l2) (add1 i)))]))
  (for* ([i (in-range maxValue 0 -1)]
         [j (in-inclusive-range (* 2 i) maxValue i)])
    (vector-set! v i (list-combine (vector-ref v i) (cons 0 (vector-ref v j)))))
  (define l
    (for/fold ([l '(0)])
              ([vi (in-vector v 1)])
      (list-combine l vi)))
  (define a
    (for/sum ([(li i) (in-indexed l)])
      (* li (binomial (sub1 n) i))))
  (remainder a 1000000007))

(ideal-arrays 2 5)
(ideal-arrays 5 3)
(ideal-arrays 3 9)
