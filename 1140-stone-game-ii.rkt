#lang racket

(define/contract (stone-game-ii piles)
  (-> (listof exact-integer?) exact-integer?)
  (define piles-vec (list->vector piles))
  (define n (vector-length piles-vec))
  (define dp (build-vector (add1 n) (λ (_) (make-vector (add1 n) -1919810))))
  (displayln dp)
  (define (dp-ref i j) (vector-ref (vector-ref dp i) j))
  (define (dp-set! i j v) (vector-set! (vector-ref dp i) j v))
  (for* ([i (in-range n -1 -1)]
         [m (in-range 1 (add1 n))])
    (cond [(= i n) (dp-set! i m 0)]
          [else
           (for/fold ([s 0])
                     ([x (in-range 1 (add1 (* 2 m)))]
                      #:break (> (+ i x) n))
             (define new-s (+ s (vector-ref piles-vec (+ i x -1))))
             (dp-set! i m (max (dp-ref i m) (- new-s (dp-ref (+ i x) (min n (max m x))))))
             new-s)]))
  (quotient (+ (dp-ref 0 1) (apply + piles)) 2))

(stone-game-ii '(2 7 9 4 4))