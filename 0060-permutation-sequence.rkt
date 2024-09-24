#lang racket

(require math/number-theory)

(define/contract (get-permutation n k)
  (-> exact-integer? exact-integer? string?)
  (list->string
   (let recr ([a (factorial (sub1 n))] [d (range n)] [k (sub1 k)] [i (sub1 n)])
     (if (zero? i)
         (list (integer->char (+ 49 (car d))))
         (let-values ([(q r) (quotient/remainder k a)])
           (define j (list-ref d q))
           (cons (integer->char (+ 49 j))
                 (recr (/ a i) (remove j d) r (sub1 i))))))))

(get-permutation 3 3)
