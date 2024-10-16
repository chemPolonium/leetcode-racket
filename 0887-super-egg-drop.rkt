#lang racket

(define/contract (super-egg-drop k n)
  (-> exact-integer? exact-integer? exact-integer?)
  (define h (make-hash))
  ;;; calculate the maximum floor it can get
  ;;; next floor throw 0 + next floor throw 1 + ...
  (define (f k c)
    (cond [(zero? c) 0]
          [(= c 1) 1]
          [(= k 1) c]
          [else
           (hash-ref! h (cons k c)
                      (for/sum ([i (in-range c)])
                        (add1 (f (sub1 k) i))))]))
  (let iter ([i 1])
    (if (>= (f k i) n)
        i
        (iter (add1 i)))))

(define/contract (super-egg-drop-1 k n)
  (-> exact-integer? exact-integer? exact-integer?)
  (define h (make-hash))
  ;;; calculate the maximum floor it can get
  (define (f k c)
    (cond [(zero? c) 0]
          [(= c 1) 1]
          [(= k 1) c]
          [else
           (hash-ref! h (cons k c)
                      (+ (f (sub1 k) (sub1 c))
                         (f k (sub1 c))
                         1))]))
  (let iter ([i 1])
    (if (>= (f k i) n)
        i
        (iter (add1 i)))))

;;; time out
(define/contract (super-egg-drop-2 k n)
  (-> exact-integer? exact-integer? exact-integer?)
  (define h (make-hash))
  (define (dp k n)
    (cond [(zero? n) 0]
          [(= k 1) n]
          [else
           (hash-ref! h (cons k n)
                      (let iter ([lo 1] [hi n])
                        (cond [(< (add1 lo) hi)
                               (define x (quotient (+ lo hi) 2))
                               (define t1 (dp (sub1 k) (sub1 x)))
                               (define t2 (dp k (- n x)))
                               (cond [(< t1 t2) (iter x hi)]
                                     [(> t1 t2) (iter lo x)]
                                     [else (iter x x)])]
                              [else
                               (add1 (min (max (dp (sub1 k) (sub1 lo))
                                               (dp k (- n lo)))
                                          (max (dp (sub1 k) (sub1 hi))
                                               (dp k (- n hi)))))])))]))
  (dp k n))

(super-egg-drop 4 15)
