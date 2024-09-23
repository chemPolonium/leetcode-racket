#lang racket

(require data/gvector)

(define/contract (solve-n-queens n)
  (-> exact-integer? (listof (listof string?)))
  (define gv (gvector))
  (let recr ([l null] [k 1])
    (for ([i (in-range n)]
          #:when (for/and ([(lj j) (in-indexed (in-list l))])
                   (not (or (= lj i) (= lj (+ i j 1)) (= lj (- i j 1))))))
      (define nl (cons i l))
      (if (= k n)
          (gvector-add! gv nl)
          (recr nl (add1 k)))))
  (for/list ([l (in-gvector gv)])
    (for/list ([i (in-list l)])
      (define s (make-string n #\.))
      (string-set! s i #\Q)
      s)))

(solve-n-queens 4)
