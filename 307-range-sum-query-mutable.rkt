#lang racket

(define num-array%
  (class object%
    (super-new)

    ; nums : (listof exact-integer?)
    (init-field
     nums)

    (define nums-vec (list->vector (cons 0 nums)))
    (define n (sub1 (vector-length nums-vec)))

    (define seg-tree (make-vector (add1 (* 4 n))))
    (define (l-child p) (* p 2))
    (define (r-child p) (add1 (* p 2)))
    (define (parent p) (quotient p 2))
    (define nind->tind (make-vector (add1 n)))

    (define (build-tree s t p)
      (cond [(= s t)
             (vector-set! nind->tind s p)
             (vector-set! seg-tree p (vector-ref nums-vec s))]
            [else
             (define m (quotient (+ s t) 2))
             (build-tree s m (l-child p))
             (build-tree (add1 m) t (r-child p))
             (vector-set! seg-tree p
                          (+ (vector-ref seg-tree (l-child p))
                             (vector-ref seg-tree (r-child p))))]))

    (build-tree 1 n 1)

    (define (-update index val)
      (define diff (- val (vector-ref nums-vec index)))
      (vector-set! nums-vec index val)
      (let loop ([p (vector-ref nind->tind index)])
        (vector-set! seg-tree p (+ (vector-ref seg-tree p) diff))
        (unless (= p 1)
          (loop (parent p)))))

    (define (-sum-range l r s t p)
      (cond [(and (<= l s) (<= t r))
             (vector-ref seg-tree p)]
            [else
             (define m (quotient (+ s t) 2))
             (+ (if (<= l m) (-sum-range l r s m (l-child p)) 0)
                (if (> r m) (-sum-range l r (add1 m) t (r-child p)) 0))]))

    ; update : exact-integer? exact-integer? -> void?
    (define/public (update index val)
      (-update (add1 index) val))

    ; sum-range : exact-integer? exact-integer? -> exact-integer?
    (define/public (sum-range left right)
      (-sum-range (add1 left) (add1 right) 1 n 1))))

;; Your num-array% object will be instantiated and called as such:
;; (define obj (new num-array% [nums nums]))
;; (send obj update index val)
;; (define param_2 (send obj sum-range left right))

; (define obj (new num-array% [nums '(1 3 5)]))
; (send obj sum-range 0 2)
; (send obj update 1 2)
; (send obj sum-range 0 2)

; (define obj (new num-array% [nums '(-1)]))
; (send obj sum-range 0 0)
; (send obj update 0 1)
; (send obj sum-range 0 0)

(define obj (new num-array% [nums '(9 -8)]))
(send obj update 0 3)
(send obj sum-range 1 1)
(send obj sum-range 0 1)
(send obj update 1 -3)
(send obj sum-range 0 1)
