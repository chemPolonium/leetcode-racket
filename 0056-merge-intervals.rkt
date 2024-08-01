#lang racket

(define/contract (merge intervals)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (define (extract-interval intervals start end)
    (if (or (null? intervals) (<= (add1 end) (caar intervals)))
        (values (list start end) intervals)
        (extract-interval (cdr intervals) start (max end (cadar intervals)))))
  (let iter ([intervals (sort intervals < #:key car)])
    (cond [(null? intervals) null]
          [else
           (define-values (first-interval rest-intervals)
             (extract-interval (cdr intervals) (caar intervals) (cadar intervals)))
           (cons first-interval (iter rest-intervals))])))

(merge '((1 3) (2 6) (8 10) (15 18)))
