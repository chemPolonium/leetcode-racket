#lang racket

(define (vector-bs vec v [lo 0] [hi (vector-length vec)])
  (and (<= (vector-ref vec lo) v)
       (<= v (vector-ref vec (sub1 hi)))
       (let iter ([lo lo] [hi hi])
         (and (not (= lo hi))
              (let* ([mi (quotient (+ lo hi) 2)]
                     [m (vector-ref vec mi)])
                (cond [(< m v) (iter (add1 mi) hi)]
                      [(= m v) mi]
                      [else (iter lo (sub1 mi))]))))))

; (vector-bs #(1 3 5 7 9) 0)
; (vector-bs #(1 3 5 7 9) 3)
; (vector-bs #(1 3 5 7 9) 5)
; (vector-bs #(1 3 5 7 9) 10)

; (define (vector-lb vec v [lo 0] [hi (vector-length vec)])
;   (cond [(<= v (vector-ref vec lo)) lo]
;         [(< (vector-ref vec (sub1 hi)) v) #f]
;         [else
;          (let iter ([lo lo] [hi hi])
;            (if (= (add1 lo) hi)
;                hi
;                (let* ([mi (quotient (+ lo hi 1) 2)]
;                       [m (vector-ref vec mi)])
;                  (if (< m v)
;                      (iter mi hi)
;                      (iter lo mi)))))]))

(define (vector-lb vec v [lo 0] [hi (vector-length vec)])
  (cond [(<= v (vector-ref vec lo)) lo]
        [(< (vector-ref vec (sub1 hi)) v) #f]
        [else
         (let iter ([current-ind lo] [hl-dis (- hi lo)])
           (if (positive? hl-dis)
               (let* ([step (quotient hl-dis 2)]
                      [next-ind (+ current-ind step)]
                      [i (vector-ref vec next-ind)])
                 (if (< i v)
                     (iter (add1 next-ind) (- hl-dis step 1))
                     (iter current-ind step)))
               current-ind))]))

; (define v (build-vector 10000000 identity))
; (time (for ([i (in-range 10000000)])
;         (vector-lb v i)))

; (vector-lb #(1 3 5 7 9) 0)
; (vector-lb #(1 3 5 7 9) 3)
; (vector-lb #(1 3 5 7 9) 4)
; (vector-lb #(1 3 5 7 9) 10)