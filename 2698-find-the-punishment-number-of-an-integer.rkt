#lang racket

(define/contract (punishment-number n)
  (-> exact-integer? exact-integer?)
  (define (good? x)
    (define s (number->string (* x x)))
    (define n (string-length s))
    (define (subnum i j)
      (string->number (substring s i j)))
    (let loop ([i 0] [acc 0])
      (if (= i n)
          (= acc x)
          (for/or ([j (in-inclusive-range (add1 i) n)])
            (loop j (+ acc (subnum i j)))))))
  (apply + (map (λ (x) (* x x)) (filter good? (inclusive-range 1 n)))))

(punishment-number 10)
