#lang racket

(define/contract (solve-n-queens n)
  (-> exact-integer? (listof (listof string?)))
  (define ans
    (let iter ([l null] [k 0] [i 0] [ans null])
      (cond [(and (zero? k) (= i n)) ans]
            [(= i n) (iter (cdr l) (sub1 k) (add1 (car l)) ans)]
            [(= k n) (iter (cdr l) (sub1 k) (add1 (car l)) (cons l ans))]
            [(for/and ([(lj j) (in-indexed (in-list l))])
               (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1)))))
             (iter (cons i l) (add1 k) 0 ans)]
            [else (iter l k (add1 i) ans)])))
  (for/list ([l (in-list ans)])
    (for/list ([i (in-list l)])
      (define s (make-string n #\.))
      (string-set! s i #\Q)
      s)))

; (require data/gvector)
; (define/contract (solve-n-queens n)
;   (-> exact-integer? (listof (listof string?)))
;   (define gv (gvector))
;   (let recr ([l null] [k 1])
;     (for ([i (in-range n)]
;           #:when (for/and ([(lj j) (in-indexed (in-list l))])
;                    (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1))))))
;       (define nl (cons i l))
;       (if (= k n)
;           (gvector-add! gv nl)
;           (recr nl (add1 k)))))
;   (for/list ([l (in-gvector gv)])
;     (for/list ([i (in-list l)])
;       (define s (make-string n #\.))
;       (string-set! s i #\Q)
;       s)))

(solve-n-queens 4)
