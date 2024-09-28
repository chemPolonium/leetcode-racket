#lang racket

(define book-my-show%
  (class object%
    (super-new)

    ; n : exact-integer?
    ; m : exact-integer?
    (init-field
     n
     m)

    (define min-tree (make-vector (* 4 n)))
    (define sum-tree (make-vector (* 4 n)))

    (define (modify i l r ind val)
      (cond [(= l r)
             (vector-set! min-tree i val)
             (vector-set! sum-tree i val)]
            [else
             (define mid (quotient (+ l r) 2))
             (if (<= ind mid)
                 (modify (* i 2) l mid ind val)
                 (modify (add1 (* i 2)) (add1 mid) r ind val))
             (vector-set! min-tree i (min (vector-ref min-tree (* i 2))
                                          (vector-ref min-tree (add1 (* i 2)))))
             (vector-set! sum-tree i (+ (vector-ref sum-tree (* i 2))
                                        (vector-ref sum-tree (add1 (* i 2)))))]))

    (define (query-min-row i l r val)
      (cond [(and (= l r) (> (vector-ref min-tree i) val))
             n]
            [(= l r)
             l]
            [else
             (define mid (quotient (+ l r) 2))
             (if (<= (vector-ref min-tree (* 2 i)) val)
                 (query-min-row (* 2 i) l mid val)
                 (query-min-row (add1 (* 2 i)) (add1 mid) r val))]))

    (define (query-sum i l r l2 r2)
      (cond [(or (< r l2) (> l r2))
             0]
            [(and (>= l l2) (<= r r2))
             (vector-ref sum-tree i)]
            [else
             (define mid (quotient (+ l r) 2))
             (+ (query-sum (* 2 i) l mid l2 r2)
                (query-sum (add1 (* 2 i)) (add1 mid) r l2 r2))]))

    ; gather : exact-integer? exact-integer? -> (listof exact-integer?)
    (define/public (gather k max-row)
      (define i (query-min-row 1 0 (sub1 n) (- m k)))
      (if (> i max-row)
          null
          (let ([used (query-sum 1 0 (sub1 n) i i)])
            (modify 1 0 (sub1 n) i (+ used k))
            (list i used))))

    ; scatter : exact-integer? exact-integer? -> boolean?
    (define/public (scatter k max-row)
      (define used-total (query-sum 1 0 (sub1 n) 0 max-row))
      (if (> k (- (* m (add1 max-row)) used-total))
          false
          (let ([i (query-min-row 1 0 (sub1 n) (sub1 m))])
            (let iter ([i i] [k k])
              (define used (query-sum 1 0 (sub1 n) i i))
              (if (>= (- m used) k)
                  (modify 1 0 (sub1 n) i (+ used k))
                  (let ([k (- k (- m used))])
                    (modify 1 0 (sub1 n) i m)
                    (iter (add1 i) k))))
            true)))
    ))

;; Your book-my-show% object will be instantiated and called as such:
;; (define obj (new book-my-show% [n n] [m m]))
;; (define param_1 (send obj gather k max-row))
;; (define param_2 (send obj scatter k max-row))
