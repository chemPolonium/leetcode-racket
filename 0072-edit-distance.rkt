#lang racket

(define/contract (min-distance word1 word2)
  (-> string? string? exact-integer?)
  (define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
  (define (vec2d-ref vec m n) (vector-ref (vector-ref vec m) n))
  (define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))
  (define l1 (string-length word1))
  (define l2 (string-length word2))
  (define dp (make-vec2d l1 l2))
  (define/match (dp-ref i j)
    [(_ -1) (add1 i)]
    [(-1 _) (add1 j)]
    [(_ _) (vec2d-ref dp i j)])
  (for* ([i (in-range l1)]
         [j (in-range l2)])
    ;;; this should be faster because it saved some dp-ref
    ;;; but this is slower
    ;;; I don't know why
    ; (define m
    ;   (if (char=? (string-ref word1 i) (string-ref word2 j))
    ;       (dp-ref (sub1 i) (sub1 j))
    ;       (add1 (min (dp-ref (sub1 i) j)
    ;                  (dp-ref i (sub1 j))
    ;                  (dp-ref (sub1 i) (sub1 j))))))
    (define same
      (if (char=? (string-ref word1 i) (string-ref word2 j))
          0
          1))
    (define m
      (min (add1 (dp-ref (sub1 i) j))
           (add1 (dp-ref i (sub1 j)))
           (+ same (dp-ref (sub1 i) (sub1 j)))))
    (vec2d-set! dp i j m))
  (dp-ref (sub1 l1) (sub1 l2)))

(min-distance "horse" "ros")
(min-distance "" "")
