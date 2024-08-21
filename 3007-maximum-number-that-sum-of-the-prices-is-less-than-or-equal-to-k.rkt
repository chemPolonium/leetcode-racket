#lang racket

(define/contract (find-maximum-number k x)
  (-> exact-integer? exact-integer? exact-integer?)
  (define (val n)
    (define n+1 (add1 n))
    ;;; recursive version
    (let iter ([m 1] [acc 0])
      (define d/2 (expt 2 (sub1 (* m x))))
      (define d (* 2 d/2))
      (define-values (q r) (quotient/remainder n+1 d))
      (define r-val (max (- r d/2) 0))
      (define q-val (* q d/2))
      (if (= q-val 0)
          (+ acc r-val)
          (iter (add1 m)
                (+ acc r-val q-val))))
    ;;; for version
    ; (for/sum ([m (in-range 1 (add1 (/ (log n+1 2) x)))])
    ;   (define d (expt 2 (* m x)))
    ;   (define d/2 (/ d 2))
    ;   (define-values (q r) (quotient/remainder n+1 d))
    ;   (+ (* q d/2) (max (- r d/2) 0)))
    )
  (define (search-up n)
    (if (> (val n) k)
        (search-down (quotient n 2) n)
        (search-up (* n 2))))
  (define (search-down i j)
    (if (= i (sub1 j))
        i
        (let* ([m (quotient (+ i j) 2)]
               [mval (val m)])
          (if (> mval k)
              (search-down i m)
              (search-down m j)))))
  (search-up 1))

(find-maximum-number 9 1)
