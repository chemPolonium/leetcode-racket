#lang racket

(define/contract (minimum-delete-sum s1 s2)
  (-> string? string? exact-integer?)
  (define (char-sum s)
    (for/sum ([c (in-string s)])
      (char->integer c)))
  (define a (+ (char-sum s1) (char-sum s2)))
  (define vdp (make-vector (string-length s2) a))
  (for*/fold ([h a])
             ([(ci i) (in-indexed (in-string s1))]
              [(cj j) (in-indexed (in-string s2))])
    (define u (vector-ref vdp j))
    (define l (if (zero? j) a (vector-ref vdp (sub1 j))))
    (define ul (- (if (zero? j) a h) (if (char=? ci cj) (* 2 (char->integer ci)) 0)))
    (vector-set! vdp j (min u l ul))
    u)
  (vector-ref vdp (sub1 (vector-length vdp))))

(minimum-delete-sum "delete" "leet")
