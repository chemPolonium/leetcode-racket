#lang racket

(define/contract (get-folder-names names)
  (-> (listof string?) (listof string?))
  (for/fold ([h (hash)] [l null] #:result (reverse l))
            ([n (in-list names)])
    (if (not (hash-has-key? h n))
        (values (hash-set h n 1) (cons n l))
        (let iter ([k (hash-ref h n)])
          (define nn (string-append n "(" (number->string k) ")"))
          (if (not (hash-has-key? h nn))
              (values (hash-set* h nn 1 n (add1 k)) (cons nn l))
              (iter (add1 k)))))))

(get-folder-names '("pes" "fifa" "gta" "pes(2019)"))