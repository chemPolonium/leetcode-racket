#lang racket

(define/contract (max-product words)
  (-> (listof string?) exact-integer?)
  (define (char->alphabet c)
    (- (char->integer c) 97))
  (define (word->int word)
    (for/fold ([acc 0])
              ([c (in-string word)])
      (bitwise-ior acc
                   (arithmetic-shift 1 (char->alphabet c)))))
  (define word-length-vec (list->vector (map string-length words)))
  (define alphabet-vec (list->vector (map word->int words)))
  (define n (vector-length word-length-vec))
  (for*/fold ([m 0])
             ([i (in-range n)]
              [j (in-range (add1 i) n)]
              #:when (zero? (bitwise-and (vector-ref alphabet-vec i)
                                         (vector-ref alphabet-vec j))))
    (max m (* (vector-ref word-length-vec i)
              (vector-ref word-length-vec j)))))

(max-product '("abcw" "baz" "foo" "bar" "xtfn" "abcdef"))
