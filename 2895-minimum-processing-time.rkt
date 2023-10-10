#lang racket

(define/contract (min-processing-time processorTime tasks)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (for/fold ([m 0])
            ([p (in-list (sort processorTime >))]
             [t (in-slice 4 (in-list (sort tasks <)))])
    (define n (+ p (fourth t)))
    (max m n)))

(min-processing-time '(8 10) '(2 2 3 1 8 7 4 5))
(min-processing-time '(10 20) '(2 3 1 2 5 8 4 3))
