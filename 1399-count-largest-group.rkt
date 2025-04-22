#lang racket

(define/contract (count-largest-group n)
  (-> exact-integer? exact-integer?)
  (define (digit-sum x)
    (define-values (q r) (quotient/remainder x 10))
    (if (zero? q) r (+ r (digit-sum q))))
  (define h
    (for/fold ([h (hasheq)])
              ([i (in-inclusive-range 1 n)])
      (hash-update h (digit-sum i) add1 0)))
  (define h1
    (for/fold ([h1 (hasheq)])
              ([v (in-hash-values h)])
      (hash-update h1 v add1 0)))
  (hash-ref h1 (apply max (hash-keys h1))))

; a bit slower but shorter
(define/contract (count-largest-group-1 n)
  (-> exact-integer? exact-integer?)
  (define (digit-sum x)
    (apply + (map (lambda (y) (- (char->integer y) 48))
                  (string->list (number->string x)))))
  (define l (map length (group-by digit-sum (inclusive-range 1 n))))
  (count (lambda (x) (= x (apply max l))) l))
