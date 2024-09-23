#lang racket

(define/contract (total-n-queens n)
  (-> exact-integer? exact-integer?)
  (define ans 0)
  (let recr ([l null] [k 1])
    (for ([i (in-range n)]
          #:when (for/and ([(lj j) (in-indexed (in-list l))])
                   (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1))))))
      (if (= k n)
          (set! ans (add1 ans))
          (recr (cons i l) (add1 k)))))
  ans)

; slow version, many 0 added
; (define/contract (total-n-queens n)
;   (-> exact-integer? exact-integer?)
;   (let recr ([l null] [k 1])
;     (for/sum ([i (in-range n)]
;               #:when (for/and ([(lj j) (in-indexed (in-list l))])
;                        (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1))))))
;       (if (= k n)
;           1
;           (recr (cons i l) (add1 k))))))

(total-n-queens 4)
