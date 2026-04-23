#lang racket

(define/contract (two-edit-words queries dictionary)
  (-> (listof string?) (listof string?) (listof string?))
  (define trie (make-hasheq))
  (for ([d (in-list dictionary)])
    (for/fold ([st trie])
              ([c (in-string d)])
      (hash-ref! st c (make-hasheq))))
  (define (sub-match t q qpos r)
    (cond [(= r 3) #f]
          [(= qpos (string-length q)) #t]
          [else
           (for/or ([(k v) (in-hash t)])
             (define rr (if (char=? k (string-ref q qpos)) r (add1 r)))
             (sub-match v q (add1 qpos) rr))]))
  (define (single-query q)
    (sub-match trie q 0 0))
  (filter single-query queries))

(two-edit-words '("prfturjd" "iarapqqk" "aokbrtmx" "yafmjorj" "larakqqk" "nliynmpm" "isikkcws" "laraeqqk")
                '("apahhijt" "larapqqk" "isukkcws" "siqqoacj" "nloynmpm"))
