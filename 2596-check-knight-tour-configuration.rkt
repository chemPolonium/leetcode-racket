#lang racket

(define/contract (check-valid-grid grid)
  (-> (listof (listof exact-integer?)) boolean?)
  (define n ((λ (x) (* x x)) (length grid)))
  (define r-vec (make-vector n))
  (define c-vec (make-vector n))
  (for* ([(l r) (in-indexed (in-list grid))]
         [(i c) (in-indexed (in-list l))])
    (vector-set! r-vec i r)
    (vector-set! c-vec i c))
  (and
   (zero? (caar grid))
   (for/fold ([prev-r (vector-ref r-vec 0)]
              [prev-c (vector-ref c-vec 0)]
              #:result (and prev-r #t))
             ([r (sequence-tail (in-vector r-vec) 1)]
              [c (sequence-tail (in-vector c-vec) 1)]
              #:break (not prev-r))
     (define dr (abs (- r prev-r)))
     (define dc (abs (- c prev-c)))
     (if (= 2 (* dr dc))
         (values r c)
         (values #f #f)))))

(check-valid-grid '((24 11 22 17 4)
                    (21 16 5 12 9)
                    (6 23 10 3 18)
                    (15 20 1 8 13)
                    (0 7 14 19 2)))
