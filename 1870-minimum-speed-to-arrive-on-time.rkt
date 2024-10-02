#lang racket

(define (min-speed-on-time dist hour)
  (define n (length dist))
  (define (total-hour speed)
    (for/sum ([(d i) (in-indexed (in-list dist))])
      (if (= (sub1 n) i)
          (/ d speed)
          (ceiling (/ d speed)))))
  (define (min-speed lo hi)
    (define mid (quotient (+ lo hi) 2))
    (cond [(= lo mid) (add1 lo)]
          [(> (* 100 (total-hour mid)) (round (* 100 hour)))
           (min-speed mid hi)]
          [else (min-speed lo mid)]))
  (cond [(< (ceiling hour) n) -1]
        [(<= (apply + dist) hour) 1]
        [else (min-speed 1 10000000)]))

; (min-speed-on-time '(1 3 2) 2.7)
(min-speed-on-time '(1 9) 1.18)
