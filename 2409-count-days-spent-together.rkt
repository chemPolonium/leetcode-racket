#lang racket

(define/contract (count-days-together arriveAlice leaveAlice arriveBob leaveBob)
  (-> string? string? string? string? exact-integer?)
  (define (date->days str)
    (define month-days-list '(31 28 31 30 31 30 31 31 30 31 30 31))
    (define month (string->number (substring str 0 2)))
    (define day (string->number (substring str 3)))
    (define month-days (apply + (take month-days-list (sub1 month))))
    (+ month-days day))
  (define arrive-day-alice (date->days arriveAlice))
  (define arrive-day-bob (date->days arriveBob))
  (define leave-day-alice (date->days leaveAlice))
  (define leave-day-bob (date->days leaveBob))
  (max 0
       (- (add1 (min leave-day-alice leave-day-bob))
          (max arrive-day-alice arrive-day-bob))))

(count-days-together "10-01" "10-31" "08-16" "08-19")