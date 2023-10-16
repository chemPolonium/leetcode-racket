#lang racket

(define/contract (shortest-beautiful-substring s k)
  (-> string? exact-integer? string?)
  (define-values (ss j)
    (for/fold ([ss 0] [ii 0])
              ([(c i) (in-indexed (in-string s))] #:break (= ss k))
      (if (char=? c #\1)
          (values (add1 ss) i)
          (values ss i))))
  (define (find-next i)
    (let iter ([i (add1 i)])
      (cond [(>= i (string-length s)) -1]
            [(char=? #\1 (string-ref s i)) i]
            [else (iter (add1 i))])))
  (define i (find-next -1))
  (define-values (ai aj)
    (let iter ([i i] [j j] [hi i] [hj j])
      (define ni (find-next i))
      (define nj (find-next j))
      (cond [(= nj -1) (values hi hj)]
            [(< (- nj ni) (- hj hi)) (iter ni nj ni nj)]
            [(= (- nj ni) (- hj hi))
             (if (string<? (substring s ni (add1 nj))
                           (substring s hi (add1 hj)))
                 (iter ni nj ni nj)
                 (iter ni nj hi hj))]
            [else (iter ni nj hi hj)])))
  (if (< ss k)
      ""
      (substring s ai (add1 aj))))

(shortest-beautiful-substring "100011001" 3)
(shortest-beautiful-substring "1011" 2)
(shortest-beautiful-substring "000" 1)
(shortest-beautiful-substring "01011101000111110" 5)
(shortest-beautiful-substring "001" 1)
