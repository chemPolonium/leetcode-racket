#lang racket

(define (vector-update! vec pos update)
  (vector-set! vec pos (update (vector-ref vec pos))))

(define ((adder x) y)
  (+ x y))

(define ((suber x) y)
  (- y x))

(define (find-max-valid valid? lo hi)
  (let iter ([lo lo] [hi hi])
    (cond [(= lo hi) lo]
          [else
           (define n (add1 (quotient (+ hi lo) 2)))
           (cond [(valid? n) (iter n hi)]
                 [else (iter lo (sub1 n))])])))

(define/contract (max-power stations r k)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer?)
  (define diff-vector (make-vector (length stations)))
  (define stations-count (vector-length diff-vector))
  (for ([(s i) (in-indexed (in-list stations))])
    (define diff-begin (max 0 (- i r)))
    (define diff-end (+ i r 1))
    (vector-update! diff-vector diff-begin (adder s))
    (when (< diff-end stations-count)
      (vector-update! diff-vector diff-end (suber s))))
  (define (power-valid? target-power)
    (define temp-diff-vector (vector-copy diff-vector))
    (let iter ([i 0] [k-rem k] [prev-power 0])
      (cond [(negative? k-rem) false]
            [(= i stations-count) true]
            [else
             (define current-power (+ (vector-ref temp-diff-vector i) prev-power))
             (cond [(< current-power target-power)
                    (define power-need (- target-power current-power))
                    (vector-update! temp-diff-vector i (adder power-need))
                    (define diff-end (+ i (* 2 r) 1))
                    (when (< diff-end stations-count)
                      (vector-update! temp-diff-vector diff-end (suber power-need)))
                    (iter (add1 i) (- k-rem power-need) target-power)]
                   [else
                    (iter (add1 i) k-rem current-power)])])))
  (find-max-valid power-valid? 0 (+ k (apply + stations))))

; (max-power '(1 2 4 5 0) 1 2)

; (max-power '(4 4 4 4) 0 3)

; (max-power '(4 2) 1 1)

(max-power '(2 10 12 3) 0 14)