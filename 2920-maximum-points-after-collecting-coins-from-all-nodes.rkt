#lang racket

(define/contract (maximum-points edges coins k)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) exact-integer? exact-integer?)
  (define coins-vec (list->vector coins))
  (define n (vector-length coins-vec))
  (define g (make-vector n null))
  (for ([e (in-list edges)])
    (match e
      [(list a b)
       (vector-set! g a (cons b (vector-ref g a)))
       (vector-set! g b (cons a (vector-ref g b)))]))
  (define mem-hash (make-hash))
  (define (dfs i j fa)
    (cond [(hash-has-key? mem-hash (list* i j fa))
           (hash-ref mem-hash (list* i j fa))]
          [else
           (define v
             (for/fold ([res1 (- (arithmetic-shift (vector-ref coins-vec i) j) k)]
                        [res2 (arithmetic-shift (vector-ref coins-vec i) (sub1 j))]
                        #:result (max res1 res2))
                       ([ch (in-list (vector-ref g i))]
                        #:unless (= ch fa))
               (values (+ res1 (dfs ch j i))
                       (if (> j -13)
                           (+ res2 (dfs ch (sub1 j) i))
                           res2))))
           (hash-set! mem-hash (list* i j fa) v)
           v]))
  (dfs 0 0 -1))

; (maximum-points '((0 1) (1 2) (2 3)) '(10 10 3 3) 5)
; (maximum-points '((0 1) (0 2)) '(8 4 4) 0)
(maximum-points '((0 1) (0 2) (0 3) (2 4) (5 4) (6 0) (4 7) (8 5))
                '(2 3 10 0 0 2 7 3 9)
                2)
