#lang racket

; (define/contract (minimum-jumps forbidden a b x)
;   (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer? exact-integer?)
;   (define lower 0)
;   (define upper (+ b (max x (+ a (apply max forbidden)))))
;   (define forbidden-set (list->seteq forbidden))
;   (define forward-visited-set (mutable-seteq 0))
;   (define back-visited-set (mutable-seteq 0))
;   (define prev-forward-visited-set (mutable-seteq 0))
;   (define prev-back-visited-set (mutable-seteq))
;   (define current-forward-visit-set (mutable-seteq))
;   (define current-back-visit-set (mutable-seteq))
;   (let iter ([jumps 1]
;              [forward-unvisited? #t])
;     (cond [(= x 0) 0]
;           [(and (not (set-empty? prev-forward-visited-set))
;                 forward-unvisited?)
;            (define prev-visited (set-first prev-forward-visited-set))
;            (define forward-visit (+ prev-visited a))
;            (cond [(or (set-member? forbidden-set forward-visit)
;                       (set-member? forward-visited-set forward-visit)
;                       (> forward-visit upper))
;                   (iter jumps false)]
;                  [(= forward-visit x) jumps]
;                  [else
;                   (set-add! forward-visited-set forward-visit)
;                   (set-add! back-visited-set forward-visit)
;                   (set-add! current-forward-visit-set forward-visit)
;                   (iter jumps false)])]
;           [(not (set-empty? prev-forward-visited-set))
;            (define prev-visited (set-first prev-forward-visited-set))
;            (define back-visit (- prev-visited b))
;            (cond [(or (set-member? forbidden-set back-visit)
;                       (set-member? back-visited-set back-visit)
;                       (< back-visit lower))
;                   (set-remove! prev-forward-visited-set prev-visited)
;                   (iter jumps true)]
;                  [(= back-visit x) jumps]
;                  [else
;                   (set-add! back-visited-set back-visit)
;                   (set-add! current-back-visit-set back-visit)
;                   (set-remove! prev-forward-visited-set prev-visited)
;                   (iter jumps true)])]
;           [(not (set-empty? prev-back-visited-set))
;            (define prev-visited (set-first prev-back-visited-set))
;            (define forward-visit (+ prev-visited a))
;            (cond [(or (set-member? forbidden-set forward-visit)
;                       (set-member? forward-visited-set forward-visit)
;                       (> forward-visit upper))
;                   (set-remove! prev-back-visited-set prev-visited)
;                   (iter jumps false)]
;                  [(= forward-visit x) jumps]
;                  [else
;                   (set-add! forward-visited-set forward-visit)
;                   (set-add! back-visited-set forward-visit)
;                   (set-add! current-forward-visit-set forward-visit)
;                   (set-remove! prev-back-visited-set prev-visited)
;                   (iter jumps false)])]
;           [(and (set-empty? current-forward-visit-set)
;                 (set-empty? current-back-visit-set))
;            -1]
;           [else
;            (set! prev-forward-visited-set current-forward-visit-set)
;            (set! prev-back-visited-set current-back-visit-set)
;            (set! current-forward-visit-set (mutable-seteq))
;            (set! current-back-visit-set (mutable-seteq))
;            (iter (add1 jumps) true)])))

(define/contract (minimum-jumps forbidden a b x)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer? exact-integer?)
  (define lower 0)
  (define upper (+ b (max x (+ a (apply max forbidden)))))
  (define forbidden-set (list->seteq forbidden))
  (let iter ([jumps 1]
             [forward-unvisited? #t]
             [forward-visited-set (seteq 0)]
             [back-visited-set (seteq 0)]
             [prev-forward-visited-set (seteq 0)]
             [prev-back-visited-set (seteq)]
             [current-forward-visit-set (seteq)]
             [current-back-visit-set (seteq)])
    (cond [(= x 0) 0]
          [(and (not (set-empty? prev-forward-visited-set))
                forward-unvisited?)
           (define prev-visited (set-first prev-forward-visited-set))
           (define forward-visit (+ prev-visited a))
           (cond [(or (set-member? forbidden-set forward-visit)
                      (set-member? forward-visited-set forward-visit)
                      (> forward-visit upper))
                  (iter jumps
                        false
                        forward-visited-set
                        back-visited-set
                        prev-forward-visited-set
                        prev-back-visited-set
                        current-forward-visit-set
                        current-back-visit-set)]
                 [(= forward-visit x) jumps]
                 [else
                  (iter jumps
                        false
                        (set-add forward-visited-set forward-visit)
                        (set-add back-visited-set forward-visit)
                        prev-forward-visited-set
                        prev-back-visited-set
                        (set-add current-forward-visit-set forward-visit)
                        current-back-visit-set)])]
          [(not (set-empty? prev-forward-visited-set))
           (define prev-visited (set-first prev-forward-visited-set))
           (define back-visit (- prev-visited b))
           (cond [(or (set-member? forbidden-set back-visit)
                      (set-member? back-visited-set back-visit)
                      (< back-visit lower))
                  (iter jumps
                        true
                        forward-visited-set
                        back-visited-set
                        (set-rest prev-forward-visited-set)
                        prev-back-visited-set
                        current-forward-visit-set
                        current-back-visit-set)]
                 [(= back-visit x) jumps]
                 [else
                  (iter jumps
                        true
                        forward-visited-set
                        (set-add back-visited-set back-visit)
                        (set-rest prev-forward-visited-set)
                        prev-back-visited-set
                        current-forward-visit-set
                        (set-add current-back-visit-set back-visit))])]
          [(not (set-empty? prev-back-visited-set))
           (define prev-visited (set-first prev-back-visited-set))
           (define forward-visit (+ prev-visited a))
           (cond [(or (set-member? forbidden-set forward-visit)
                      (set-member? forward-visited-set forward-visit)
                      (> forward-visit upper))
                  (iter jumps
                        false
                        forward-visited-set
                        back-visited-set
                        prev-forward-visited-set
                        (set-rest prev-back-visited-set)
                        current-forward-visit-set
                        current-back-visit-set)]
                 [(= forward-visit x) jumps]
                 [else
                  (iter jumps
                        false
                        (set-add forward-visited-set forward-visit)
                        (set-add back-visited-set forward-visit)
                        prev-forward-visited-set
                        (set-rest prev-back-visited-set)
                        (set-add current-forward-visit-set forward-visit)
                        current-back-visit-set)])]
          [(and (set-empty? current-forward-visit-set)
                (set-empty? current-back-visit-set))
           -1]
          [else (iter (add1 jumps)
                      true
                      forward-visited-set
                      back-visited-set
                      current-forward-visit-set
                      current-back-visit-set
                      (seteq)
                      (seteq))])))

(minimum-jumps '(14 4 18 1 15) 3 15 9)

(minimum-jumps '(8 3 16 6 12 20) 15 13 11)

(minimum-jumps '(1 6 2 14 5 17 4) 16 9 7)

(minimum-jumps '(3) 14 5 90)
