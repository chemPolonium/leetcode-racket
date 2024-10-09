#lang racket

(define/contract (minimum-difference nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define nums-vec (list->vector nums))
  (for/fold ([m 1000000001])
            ([(nj j) (in-indexed (in-list nums))])
    (min m
         (abs (- nj k))
         (for/fold ([m m])
                   ([i (in-range (sub1 j) -1 -1)]
                    #:do [(define ni (vector-ref nums-vec i))]
                    #:break (= ni (bitwise-ior ni nj)))
           (define nni (bitwise-ior ni nj))
           (vector-set! nums-vec i nni)
           (min m (abs (- nni k)))))))

; (define/contract (minimum-difference nums k)
;   (-> (listof exact-integer?) exact-integer? exact-integer?)
;   (define nums-vec (list->vector nums))
;   (define n (vector-length nums-vec))
;   (define cs 0)
;   (define v (make-vector 31))
;   (define (add-num x)
;     (let iter ([i 0] [r x] [p 1])
;       (unless (zero? r)
;         (define-values (q b) (quotient/remainder r 2))
;         (when (= b 1)
;           (when (zero? (vector-ref v i))
;             (set! cs (+ cs p)))
;           (vector-set! v i (add1 (vector-ref v i))))
;         (iter (add1 i) q (* 2 p)))))
;   (define (sub-num x)
;     (let iter ([i 0] [r x] [p 1])
;       (unless (zero? r)
;         (define-values (q b) (quotient/remainder r 2))
;         (when (= b 1)
;           (when (= 1 (vector-ref v i))
;             (set! cs (- cs p)))
;           (vector-set! v i (sub1 (vector-ref v i))))
;         (iter (add1 i) q (* 2 p)))))
;   (let iter ([i 0] [j 0] [m 1000000001])
;     (cond [(and (< cs k) (= j n)) m]
;           [(and (> cs k) (= i (sub1 n))) m]
;           [(= cs k) 0]
;           [(< cs k)
;            (add-num (vector-ref nums-vec j))
;            (iter i (add1 j) (min m (abs (- k cs))))]
;           [(<= (- j i) 1)
;            (add-num (vector-ref nums-vec j))
;            (iter i (add1 j) m)]
;           [else
;            (sub-num (vector-ref nums-vec i))
;            (iter (add1 i) j (min m (abs (- k cs))))])))

(minimum-difference '(1 2 4 5) 3)
(minimum-difference '(1 3 1 3) 2)
(minimum-difference '(1) 10)
(minimum-difference '(3 3 8) 8)
(minimum-difference '(3 50 1 29 27) 66)
