#lang racket

(define/contract (four-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof (listof exact-integer?)))
  (define v (list->vector (sort nums <)))
  (define a empty)
  (define (n-sum n k target end)
    (if (= n 2)
        (let iter ([i 0] [j (sub1 k)] [prev-vi -1000000001] [prev-vj 1000000001])
          (unless (>= i j)
            (define vi (vector-ref v i))
            (define vj (vector-ref v j))
            (define svij (+ vi vj))
            (cond [(or (= prev-vi vi) (< svij target)) (iter (add1 i) j vi prev-vj)]
                  [(or (= prev-vj vj) (> svij target)) (iter i (sub1 j) prev-vi vj)]
                  [else
                   (set! a (cons (list* vi vj end) a))
                   (iter (add1 i) (sub1 j) vi vj)])))
        (let iter ([i (sub1 k)] [prev-vi 1000000001])
          (unless (< i (sub1 n))
            (define vi (vector-ref v i))
            (cond [(= prev-vi vi) (iter (sub1 i) vi)]
                  [else
                   (n-sum (sub1 n) i (- target vi) (cons vi end))
                   (iter (sub1 i) vi)])))))
  (n-sum 4 (vector-length v) target null)
  a)

(four-sum '(1 0 -1 0 -2 2) 0)
(four-sum '(2 2 2 2 2) 8)
