#lang racket

; (define/contract (max-score nums x)
;   (-> (listof exact-integer?) exact-integer? exact-integer?)
;   (define tail-odd-sum
;     (- (car nums) (if (odd? (car nums)) 0 x)))
;   (define tail-even-sum
;     (- (car nums) (if (even? (car nums)) 0 x)))
;   (for ([n (sequence-tail (in-list nums) 1)])
;     (if (odd? n)
;         (set! tail-odd-sum (+ n (max (- tail-even-sum x) tail-odd-sum)))
;         (set! tail-even-sum (+ n (max (- tail-odd-sum x) tail-even-sum)))))
;   (max tail-odd-sum tail-even-sum))

(define/contract (max-score nums x)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define tail-odd-sum
    (- (car nums) (if (odd? (car nums)) 0 x)))
  (define tail-even-sum
    (- (car nums) (if (even? (car nums)) 0 x)))
  (for/fold ([tail-odd-sum tail-odd-sum]
             [tail-even-sum tail-even-sum]
             #:result (max tail-odd-sum tail-even-sum))
            ([n (sequence-tail (in-list nums) 1)])
    (if (odd? n)
        (values (+ n (max (- tail-even-sum x) tail-odd-sum)) tail-even-sum)
        (values tail-odd-sum (+ n (max (- tail-odd-sum x) tail-even-sum))))))

(max-score '(2 3 6 1 9 2) 5)

(max-score '(8 50 65 85 8 73 55 50 29 95 5 68 52 79) 74)
