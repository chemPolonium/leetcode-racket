#lang racket

(define/contract (similar-pairs words)
  (-> (listof string?) exact-integer?)
  (let ([h (for/fold ([h (hash)])
                     ([word (in-list words)])
             (hash-update h (list->set (string->list word)) add1 0))])
    (apply + (map (lambda (x) (/ (* x (sub1 x)) 2)) (hash-values h)))))

(similar-pairs '("aba" "aabb" "abcd" "bac" "aabc"))

(similar-pairs '("aabb" "ab" "ba"))