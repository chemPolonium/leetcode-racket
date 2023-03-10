#lang racket

(define/contract (min-subarray nums p)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let ([x (modulo (apply + nums) p)])
    (displayln x)
    (if (zero? x)
        0
        (for/fold ([y 0]
                   [res (length nums)]
                   [index (hasheq 0 -1)]
                   #:result (if (= res (length nums)) -1 res))
                  ([(v i) (in-indexed (in-list nums))])
          (let* ([y (modulo (+ y v) p)]
                 [res (if (hash-has-key? index (modulo (- y x) p))
                          (min res (- i (hash-ref index (modulo (- y x) p))))
                          res)]
                 [index (hash-set index y i)])
            (values y res index))))))

(min-subarray '(1 2 3) 7)