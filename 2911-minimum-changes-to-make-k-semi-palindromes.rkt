#lang racket

(require math/number-theory)

(define/contract (minimum-changes s k)
  (-> string? exact-integer? exact-integer?)
  (define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))
  (define (vec2d-update! vec m n updater) (vec2d-set! vec m n (updater (vec2d-ref vec m n))))
  (define (modify s)
    (define n (string-length s))
    (for/fold ([m 114514])
              ([d (in-list (divisors n))] #:break (= d n))
      (define md
        (for/sum ([i0 (in-range d)] [j0 (in-range (- n d) n)])
          (let loop ([i i0] [j j0] [acc 0])
            (if (>= i j)
                acc
                (let* ([si (string-ref s i)]
                       [sj (string-ref s j)]
                       [c (if (char=? si sj) 0 1)])
                  (loop (+ i d) (- j d) (+ acc c)))))))
      (min m md)))
  (define n (string-length s))
  (define modify-grid (make-vec2d n (add1 n)))
  (for* ([i (in-inclusive-range 0 n)]
         [j (in-inclusive-range (+ i 2) n)])
    (vec2d-set! modify-grid i j (modify (substring s i j))))
  (define v1 (build-vector (add1 n) (λ (i) (vec2d-ref modify-grid 0 i))))
  (define v2 (make-vector (add1 n)))
  (for ([i (in-inclusive-range 2 k)])
    (for ([j (in-inclusive-range (* 2 i) n)])
      ; fij: i groups, j chars
      (define fij
        (for/fold ([m 114514])
                  ([j1 (in-inclusive-range (* 2 (sub1 i)) (- j 2))])
          (min m (+ (vector-ref v1 j1) (vec2d-ref modify-grid j1 j)))))
      (vector-set! v2 j fij))
    (vector-copy! v1 0 v2))
  (vector-ref v1 n))

(minimum-changes "abcac" 2)
(minimum-changes "abcdef" 2)
(minimum-changes "aabbaa" 3)
(minimum-changes "acba" 2)
