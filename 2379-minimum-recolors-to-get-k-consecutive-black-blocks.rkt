#lang racket

(define/contract (minimum-recolors blocks k)
  (-> string? exact-integer? exact-integer?)
  (define first-ops
    (for/sum ([i (in-range k)])
      (if (char=? #\W (string-ref blocks i)) 1 0)))
  (for/fold ([ops first-ops] [m first-ops] #:result m)
            ([i (in-range k (string-length blocks))])
    (define new-ops
      (+ ops
         (if (char=? #\W (string-ref blocks i)) 1 0)
         (if (char=? #\W (string-ref blocks (- i k))) -1 0)))
    (values new-ops (min new-ops m))))

(minimum-recolors "WBBWWBBWBW" 7)