#lang racket

(define (vector-cons-list! vec pos v)
  (vector-set! vec pos (cons v (vector-ref vec pos))))

(define/contract (max-output n edges price)
  (-> exact-integer? (listof (listof exact-integer?)) (listof exact-integer?) exact-integer?)
  (define price-vector (list->vector price))
  (define edge-vector (make-vector n empty))
  (for ([e (in-list edges)])
    (match-define (list a b) e)
    (vector-cons-list! edge-vector a b)
    (vector-cons-list! edge-vector b a))

  (define ans 0)
  (let dfs ([x 0] [fa -1])
    (define max_s1 (vector-ref price-vector x))
    (define p max_s1)
    (define max_s2 0)
    (for ([y (in-list (vector-ref edge-vector x))])
      (unless (= y fa)
        (define-values (s1 s2) (dfs y x))
        (set! ans (max ans (+ max_s1 s2) (+ max_s2 s1)))
        (set! max_s1 (max max_s1 (+ s1 p)))
        (set! max_s2 (max max_s2 (+ s2 p)))))
    (values max_s1 max_s2))
  ans)

(max-output 6 '((0 1) (1 2) (1 3) (3 4) (3 5)) '(9 8 7 6 10 5))