#lang racket

(define/contract (balanced-string s)
  (-> string? exact-integer?)
  (define l (string-length s))
  (define max-qwer (/ l 4))
  (define h (make-hash))
  (for ([c (in-string s)])
    (hash-update! h c add1 0))
  (define (valid?)
    (for/and ([v (in-hash-values h)])
      (<= v max-qwer)))
  (if (valid?)
      0
      (let iter ([i 0] [j 0] [res l])
        (cond [(valid?)
               (hash-update! h (string-ref s i) add1)
               (iter (add1 i) j (min (- j i) res))]
              [(= j l) res]
              [else
               (hash-update! h (string-ref s j) sub1)
               (iter i (add1 j) res)]))))

(balanced-string "QWER")
(balanced-string "QQWE")
(balanced-string "QQQW")
(balanced-string "QQWWEERR")
(balanced-string "WQWRQQQW")