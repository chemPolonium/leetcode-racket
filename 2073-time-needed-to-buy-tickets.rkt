#lang racket

(define/contract (time-required-to-buy tickets k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define tk (list-ref tickets k))
  (for/sum ([(ti i) (in-indexed (in-list tickets))])
    (min ti (if (<= i k) tk (sub1 tk)))))

;;; simulation
; (require data/queue)

; (define/contract (time-required-to-buy tickets k)
;   (-> (listof exact-integer?) exact-integer? exact-integer?)
;   (define q (make-queue))
;   (for ([(ti i) (in-indexed (in-list tickets))])
;     (enqueue! q (if (= i k) (+ 100 ti) ti)))
;   (let iter ([t 0])
;     (define tf (dequeue! q))
;     (cond [(= tf 1) (iter (add1 t))]
;           [(= tf 101) (add1 t)]
;           [else
;            (enqueue! q (sub1 tf))
;            (iter (add1 t))])))
