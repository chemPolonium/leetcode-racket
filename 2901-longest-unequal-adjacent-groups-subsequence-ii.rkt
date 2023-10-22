#lang racket

(define/contract (get-words-in-longest-subsequence n words groups)
  (-> exact-integer? (listof string?) (listof exact-integer?) (listof string?))
  (define words-vec (list->vector words))
  (define groups-vec (list->vector groups))
  (define len-vec (make-vector n 1))
  (define prev-vec (make-vector n -1))
  (define (h-length=1? a b)
    (and (= (string-length a) (string-length b))
         (= 1
            (for/fold ([s 0])
                      ([c1 (in-string a)]
                       [c2 (in-string b)]
                       #:break (> s 1))
              (if (char=? c1 c2)
                  s
                  (add1 s))))))
  (define (connectable? i j)
    (and (h-length=1? (vector-ref words-vec i) (vector-ref words-vec j))
         (not (= (vector-ref groups-vec i) (vector-ref groups-vec j)))))
  (for* ([i (in-range n)]
         [j (in-range i)])
    (define li (vector-ref len-vec i))
    (define lj (vector-ref len-vec j))
    (when (and (connectable? i j) (> (add1 lj) li))
      (vector-set! len-vec i (add1 lj))
      (vector-set! prev-vec i j)))
  (define e (argmax (λ (i) (vector-ref len-vec i)) (range n)))
  (define subs
    (let loop ([i e] [l null])
      (define p (vector-ref prev-vec i))
      (define nl (cons i l))
      (if (= p -1)
          nl
          (loop p nl))))
  (map (λ (i) (vector-ref words-vec i)) subs))
