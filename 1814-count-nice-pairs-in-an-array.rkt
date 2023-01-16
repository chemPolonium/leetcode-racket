#lang racket

(define (rev x)
  (let iter ([x x] [res 0])
    (if (zero? x)
        res
        (let-values ([(q r) (quotient/remainder x 10)])
          (iter q (+ r (* res 10)))))))

(define/contract (count-nice-pairs nums)
  (-> (listof exact-integer?) exact-integer?)
  (define diff-list (map (lambda (x) (- x (rev x))) nums))
  (define good-prime (exact-ceiling (+ 7 1e9)))
  (define h (for/fold ([h (hash)])
                      ([x diff-list])
              (hash-update h x add1 0)))
  (for/fold ([ans 0])
            ([v (in-hash-values h)])
    (remainder (+ ans (/ (* v (sub1 v)) 2)) good-prime)))

(count-nice-pairs '(42 11 1 97))