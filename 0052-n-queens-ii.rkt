#lang racket

(define/contract (total-n-queens n)
  (-> exact-integer? exact-integer?)
  (let iter ([l null] [k 0] [i 0] [ans 0])
    (cond [(and (zero? k) (= i n)) ans]
          [(= i n) (iter (cdr l) (sub1 k) (add1 (car l)) ans)]
          [(= k n) (iter (cdr l) (sub1 k) (add1 (car l)) (add1 ans))]
          [(for/and ([(lj j) (in-indexed (in-list l))])
             (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1)))))
           (iter (cons i l) (add1 k) 0 ans)]
          [else (iter l k (add1 i) ans)])))

; (define/contract (total-n-queens n)
;   (-> exact-integer? exact-integer?)
;   (define ans 0)
;   (let recr ([l null] [k 1])
;     (for ([i (in-range n)]
;           #:when (for/and ([(lj j) (in-indexed (in-list l))])
;                    (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1))))))
;       (if (= k n)
;           (set! ans (add1 ans))
;           (recr (cons i l) (add1 k)))))
;   ans)

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
