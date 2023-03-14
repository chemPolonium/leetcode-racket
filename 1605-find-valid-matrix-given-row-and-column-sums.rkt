#lang racket

(define (vector-update! vec pos updater)
  (vector-set! vec pos (updater (vector-ref vec pos))))
(define (make-vec2d m n [v 0]) (build-vector m (λ (_) (make-vector n v))))
(define (vec2d-set! vec m n v) (vector-set! (vector-ref vec m) n v))

(define/contract (restore-matrix rowSum colSum)
  (-> (listof exact-integer?) (listof exact-integer?) (listof (listof exact-integer?)))
  (define rowres (list->vector rowSum))
  (define colres (list->vector colSum))
  (define rownum (vector-length rowres))
  (define colnum (vector-length colres))
  (define mat (make-vec2d rownum colnum))
  (let iter ([i 0] [j 0])
    (when (and (< i rownum) (< j colnum))
      (define v (min (vector-ref rowres i) (vector-ref colres j)))
      (define (subv x) (- x v))
      (vec2d-set! mat i j v)
      (vector-update! rowres i subv)
      (vector-update! colres j subv)
      (let ([i (if (zero? (vector-ref rowres i)) (add1 i) i)]
            [j (if (zero? (vector-ref colres j)) (add1 j) j)])
        (iter i j))))
  (vector->list (vector-map! vector->list mat)))

; (restore-matrix '(3 8) '(4 7))
; (restore-matrix '(5 7 10) '(8 6 8))
; (restore-matrix '(14 9) '(6 9 8))
(restore-matrix '(1 0) '(1))