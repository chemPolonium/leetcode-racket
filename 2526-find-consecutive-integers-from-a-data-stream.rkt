#lang racket

(define data-stream%
  (class object%
    (super-new)

    (define c 0)

    ; value : exact-integer?
    ; k : exact-integer?
    (init-field
     value
     k)

    ; consec : exact-integer? -> boolean?
    (define/public (consec num)
      (if (= num value)
          (set! c (add1 c))
          (set! c 0))
      (>= c k))))

;; Your data-stream% object will be instantiated and called as such:
;; (define obj (new data-stream% [value value] [k k]))
;; (define param_1 (send obj consec num))