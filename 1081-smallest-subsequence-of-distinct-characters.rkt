#lang racket

(define/contract (smallest-subsequence s)
  (-> string? string?)
  (define h (make-hasheq))
  (define rnum-v (make-vector 26))
  (define flag-v (make-vector 26 #f))
  (for ([c (in-string s)])
    (define i (- (char->integer c) 97))
    (vector-set! rnum-v i (add1 (vector-ref rnum-v i))))
  (list->string
    (reverse
      (for/fold ([stack null])
                ([c (in-string s)]
                 #:do [(define i (- (char->integer c) 97))
                       (vector-set! rnum-v i (sub1 (vector-ref rnum-v i)))]
                 #:unless (vector-ref flag-v i))
        (define n-stack
          (let iter ([stack stack])
            (cond [(null? stack)
                   stack]
                  [else
                   (define topc (car stack))
                   (define topi (- (char->integer topc) 97))
                   (define topn (vector-ref rnum-v topi))
                   (cond [(and (char<? c topc) (positive-integer? topn))
                          (vector-set! flag-v topi #f)
                          (iter (cdr stack))]
                         [else
                          stack])])))
        (vector-set! flag-v i #t)
        (cons c n-stack)))))

(smallest-subsequence "bcabc")
(smallest-subsequence "cbacdcbc")
(smallest-subsequence "adabcc")
(smallest-subsequence "ecbacba")
(smallest-subsequence "bcbcbcababa")
