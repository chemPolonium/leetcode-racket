#lang racket

(define/contract (hardest-worker n logs)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (for/fold ([max-work-time -1]
             [max-work-time-id -1]
             [last-leave-time 0]
             #:result max-work-time-id)
            ([l (in-list logs)])
    (match-define (list id leave-time) l)
    (define work-time (- leave-time last-leave-time))
    (cond [(< max-work-time work-time)
           (values work-time id leave-time)]
          [(> max-work-time work-time)
           (values max-work-time max-work-time-id leave-time)]
          [else
           (values max-work-time (min max-work-time-id id) leave-time)])))