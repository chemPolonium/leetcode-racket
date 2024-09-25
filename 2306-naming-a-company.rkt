#lang racket

(define/contract (distinct-names ideas)
  (-> (listof string?) exact-integer?)
  (define h (make-hash))
  (for ([idea (in-list ideas)])
    (hash-update! h (string-ref idea 0)
                  (lambda (s) (set-add s (substring idea 1)))
                  (set)))
  (* 2
     (for/sum ([c (in-combinations (hash-values h) 2)])
       (define s1 (car c))
       (define s2 (cadr c))
       (define cc (set-count (set-intersect s1 s2)))
       (* (- (set-count s1) cc) (- (set-count s2) cc)))))

(distinct-names '("coffee" "donuts" "time" "toffee"))
