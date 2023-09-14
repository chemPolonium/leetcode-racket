#lang racket

(require data/heap)

(define/contract (schedule-course courses)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define selected (make-heap (lambda (a b) (>= (first a) (first b)))))
  (for/fold ([s 0] #:result (heap-count selected))
            ([course (in-list (sort courses < #:key second))])
    (define added-s (+ s (first course)))
    (cond [(<= added-s (second course))
           (heap-add! selected course)
           added-s]
          [(zero? (heap-count selected))
           s]
          [(<= (first (heap-min selected)) (first course))
           s]
          [else
           (define removed-s (- added-s (first (heap-min selected))))
           (heap-remove-min! selected)
           (heap-add! selected course)
           removed-s])))

(schedule-course '((100 200) (200 1300) (1000 1250) (2000 3200)))
