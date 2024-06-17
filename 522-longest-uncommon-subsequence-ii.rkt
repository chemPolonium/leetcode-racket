#lang racket

(define/contract (find-lu-slength strs)
  (-> (listof string?) exact-integer?)
  (define (substr? s contained)
    (let iter ([i 0] [j 0])
      (cond [(= j (string-length contained)) true]
            [(= i (string-length s)) false]
            [(char=? (string-ref s i) (string-ref contained j))
             (iter (add1 i) (add1 j))]
            [else (iter (add1 i) j)])))
  (define sorted (sort strs > #:key string-length))
  (define (not-sub? istr i)
    (for/and ([(jstr j) (in-indexed (in-list sorted))]
              #:unless (= i j)
              #:break (< (string-length jstr) (string-length istr)))
      (not (substr? jstr istr))))
  (define uncommon
    (for/or ([(istr i) (in-indexed (in-list sorted))])
      (and (not-sub? istr i) (string-length istr))))
  (or uncommon -1))

(find-lu-slength '("aba" "cdc" "eae"))
(find-lu-slength '("aaa" "aaa" "aa"))
