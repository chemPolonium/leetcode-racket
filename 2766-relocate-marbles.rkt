#lang racket

(define/contract (relocate-marbles nums moveFrom moveTo)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?) (listof exact-integer?))
  (for/fold ([s (list->seteq nums)]
             #:result (sort (set->list s) <))
            ([from (in-list moveFrom)]
             [to (in-list moveTo)])
    (set-add (set-remove s from) to)))
