#lang racket

(define/contract (max-total-fruits fruits startPos k)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer? exact-integer?)
  (define (accessible? l r)
    (>= k (+ (- r l) (min (abs (- l startPos)) (abs (- r startPos))))))
  (let iter ([l fruits] [r fruits] [c 0] [m 0])
    (cond [(empty? r) m]
          [(accessible? (caar l) (caar r))
           (let ([c (+ c (cadar r))])
             (iter l (cdr r) c (max m c)))]
          [(= (caar l) (caar r)) (iter (cdr l) (cdr r) c m)]
          [else (iter (cdr l) r (- c (cadar l)) m)])))

; ; looks wired but works
; ; when (> startPos l r) the distance in accessible? will be (- startPos l)
; ; so when l moves into accessible range, it will wait for r to follow it
; (define/contract (max-total-fruits fruits startPos k)
;   (-> (listof (listof exact-integer?)) exact-integer? exact-integer? exact-integer?)
;   (define (accessible? l r)
;     (>= k (+ (- r l) (min (abs (- l startPos)) (abs (- r startPos))))))
;   (let iter ([l fruits] [r fruits] [c 0] [m 0])
;     (cond [(or (empty? r) (empty? l)) m]
;           [(accessible? (caar l) (caar r))
;            (let ([c (+ c (cadar r))])
;              (iter l (cdr r) c (max m c)))]
;           [(< (caar l) startPos)
;            (iter (cdr l) r (- c (cadar l)) m)]
;           [else m])))

; (max-total-fruits '((2 8) (6 3) (8 6)) 5 4)
; (max-total-fruits '((0 9) (4 1) (5 7) (6 2) (7 4) (10 9)) 5 4)
; (max-total-fruits '((0 10000)) 20000 20000)
(max-total-fruits '((0 10000)) 20000 0)
