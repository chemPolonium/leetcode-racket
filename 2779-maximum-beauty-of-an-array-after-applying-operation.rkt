#lang racket

; (define/contract (maximum-beauty nums k)
;   (-> (listof exact-integer?) exact-integer? exact-integer?)
;   (define sorted-nums (sort nums <))
;   (let iter ([a sorted-nums] [b sorted-nums] [d 1] [m 0])
;     (cond [(null? b) m]
;           [(<= (- (car b) (car a)) (* 2 k))
;            (iter a (cdr b) (add1 d) (max m d))]
;           [else
;            (iter (cdr a) b (sub1 d) m)])))

(define/contract (maximum-beauty nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define v (list->vector nums))
  (vector-sort! v <)
  (define l (vector-length v))
  (let iter ([i 0] [j 0] [d 1] [m 0])
    (cond [(= l j) m]
          [(<= (- (vector-ref v j) (vector-ref v i)) (* 2 k))
           (iter i (add1 j) (add1 d) (max m d))]
          [else
           (iter (add1 i) j (sub1 d) m)])))

(maximum-beauty '(4 6 1 2) 2)
