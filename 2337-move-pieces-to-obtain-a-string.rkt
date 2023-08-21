#lang racket

(define/contract (can-change start target)
  (-> string? string? boolean?)
  (define (f s)
    (for/list ([(c i) (in-indexed (in-string s))]
               #:when (char-alphabetic? c))
      (cons c i)))
  (define s1 (f start))
  (define t1 (f target))
  (and (= (length s1) (length t1))
       (for/and ([s (in-list s1)]
                 [t (in-list t1)])
         (and (char=? (car s) (car t))
              (or (and (char=? (car s) #\R)
                       (<= (cdr s) (cdr t)))
                  (and (char=? (car s) #\L)
                       (>= (cdr s) (cdr t))))))))
