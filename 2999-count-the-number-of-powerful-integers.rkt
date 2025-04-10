#lang racket

(define/contract (number-of-powerful-int start finish limit s)
  (-> exact-integer? exact-integer? exact-integer? string? exact-integer?)
  (define slen (string-length s))
  (define (calculate x)
    (define xlen (string-length x))
    (cond [(< xlen slen)
           0]
          [(= xlen slen)
           (if (string>=? x s) 1 0)]
          [else
           (define suffix (substring x (- xlen slen)))
           (define prelen (- xlen slen))
           (let iter ([i 0] [c 0])
             (cond [(= i prelen)
                    (if (string>=? suffix s) (add1 c) c)]
                   [(< limit (- (char->integer (string-ref x i)) 48))
                    (+ c (expt (add1 limit) (- prelen i)))]
                   [else
                    (iter (add1 i)
                          (+ c (* (- (char->integer (string-ref x i)) 48)
                                  (expt (add1 limit) (- prelen 1 i)))))]))]))
  (- (calculate (number->string finish))
     (calculate (number->string (sub1 start)))))

(number-of-powerful-int 1 6000 4 "124")
(number-of-powerful-int 15 215 6 "10")
(number-of-powerful-int 1000 2000 4 "3000")
