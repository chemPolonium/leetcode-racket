#lang racket

(define/contract (count-battleships board)
  (-> (listof (listof char?)) exact-integer?)
  (define board-grid (list->vector (map (λ (l) (list->vector (map (λ (c) (char=? c #\X)) l))) board)))
  (define (board-ref i j) (vector-ref (vector-ref board-grid i) j))
  (define (northwest? i j)
    (and (or (zero? i) (not (board-ref (sub1 i) j)))
         (or (zero? j) (not (board-ref i (sub1 j))))))
  (for*/fold ([c 0])
             ([i (in-range (vector-length board-grid))]
              [j (in-range (vector-length (vector-ref board-grid 0)))])
    (if (and (board-ref i j) (northwest? i j))
        (add1 c)
        c)))
