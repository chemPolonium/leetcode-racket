#lang racket

;;; list version, sort nums
; (define/contract (count-ways nums)
;   (-> (listof exact-integer?) exact-integer?)
;   (define sorted-nums (sort nums <))
;   (for/fold ([prev -1] [c 0] #:result c)
;             ([(n i) (in-indexed (sequence-append (in-list sorted-nums) (in-value 114514)))])
;     (define dc (if (< prev i n) 1 0))
;     (values n (+ c dc))))

;;; hash map version, sort keys
(define/contract (count-ways nums)
  (-> (listof exact-integer?) exact-integer?)
  (define h (make-hash))
  (for ([ni (in-list nums)])
    (hash-update! h ni add1 0))
  (for/fold ([pk -1] [s 0] [a 0] #:result a)
            ([k (sequence-append (sort (hash-keys h) <) (in-value 114514))])
    (define v (hash-ref h k 0))
    (values k (+ s v) (if (< pk s k) (add1 a) a))))

(count-ways '(1))
(count-ways '(1 1))
(count-ways '(6 0 3 3 6 7 2 7))
